{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module      : Server
Description : Network server operations
Portability : POSIX

Manages network operations and offerrs high level functions
to acquire listeners and connections, and to send and receive messages.
-}
module Server
  ( -- * Listener
    Listener
  , withListener
    -- * Connection
  , Connection (..)
  , forkClient
    -- * Communication
  , ClientErr (..)
  , sendClient
  , recvClient
  ) where


-- Imports --------------------------------------------------------------

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.RWS (asks)
import Control.Exception (throwIO, bracketOnError, catch, SomeException)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import Data.Functor
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.IO.Exception (IOErrorType(Interrupted, ResourceVanished, ResourceExhausted))
import Network.Socket
  ( withSocketsDo
  , AddrInfo (addrFlags, addrSocketType, addrAddress)
  , defaultHints
  , AddrInfoFlag (AI_PASSIVE)
  , SocketType (Stream)
  , getAddrInfo
  , Socket
  , openSocket
  , ServiceName
  , setSocketOption
  , SocketOption (ReuseAddr)
  , bind
  , close
  , listen
  , SockAddr
  , accept
  )
import Network.Socket.ByteString (sendAll, recv)
import System.Random
import qualified UnliftIO.Concurrent as UIOC
import qualified UnliftIO as UIO
import System.Timeout (timeout)

import Error
  ( noCandidatesErr
  , annotate
  , classify
  , Oper (..)
  , SysErr (seOper)
  )
import Log (emit, Severity (Warn))
import Constant (queryMaxLen)
import App (App, AppEnv (config))
import Config (Config(network), NetworkConfig (..))
import UnliftIO (MonadUnliftIO)


-- Syscalls -------------------------------------------------------------

-- | Annotated 'getAddrInfo' syscall which given a hostname,
-- returns a list of compatible IP addresses and ports.
--
-- Throws 'SysErr' on failure.
tryGetAddrInfo :: AddrInfo -> ServiceName -> IO [AddrInfo]
tryGetAddrInfo hints host = annotate OpAddrInfo call
  where call = getAddrInfo (Just hints) Nothing (Just host)

-- | Annotated 'openSocket' syscall which tries to obtain
-- a socket file descriptor with the given protocol.
--
-- Throws 'SysErr' on failure.
tryOpenSock :: AddrInfo -> IO Socket
tryOpenSock addr = annotate (OpOpenSock $ addrAddress addr) call
  where call = openSocket addr

-- | Annotated 'setSocketOption' syscall which given a socket,
-- modifies it's behaviour with the provided option.
--
-- Throws 'SysErr' on failure.
trySetSockOpt :: Socket -> SocketOption -> IO ()
trySetSockOpt sock opt = annotate OpSockOpt call
  where call = setSocketOption sock opt 1

-- | Annotated 'bind' syscall which given a socket,
-- tries to bind it to the specified port on 'SockAddr'.
--
-- Throws 'SysErr' on failure.
tryBindSock :: SockAddr -> Socket -> IO ()
tryBindSock addr sock = annotate (OpBind addr) call
  where call = bind sock addr

-- | Annotated 'listen' syscall which given a socket,
-- makes it available for listening to incoming connections.
--
-- The 'backlog' parameter determines the maximum size
-- of the queue that stores pending connections.
--
-- Throws 'SysErr' on failure.
tryListenSock :: AddrInfo -> Socket -> Int -> IO ()
tryListenSock addr sock backlog = annotate (OpListen $ addrAddress addr) call
  where call = listen sock backlog

-- | Annotated 'accept' syscall which given a listening socket,
-- blocks until it receives a connection request. Returns a
-- socket to send and receive messages and the peer's 'SockAddr'.
--
-- Throws 'SysErr' on failure.
tryAcceptClient :: Socket -> IO (Socket, SockAddr)
tryAcceptClient listener = annotate OpAccept call
  where call = accept listener

-- | Annotated 'sendAll' syscall which given a socket,
-- blocks until the whole provided message is sent through.
--
-- Throws 'SysErr' on failure.
trySendAll :: SockAddr -> Socket -> ByteString -> IO ()
trySendAll addr sock msg = annotate (OpSend addr) call
  where call = sendAll sock msg

-- | Annotated 'recv' syscall which given a socket,
-- blocks until a message from the peer is received
-- and returns a buffer with the contents.
--
-- The 'chunkSize' parameter determines the maximum
-- amount of bytes that can be written into the buffer.
--
-- Throws 'SysErr' on failure.
tryRecv :: SockAddr -> Socket -> Int -> IO ByteString
tryRecv addr sock chunkSize = annotate (OpRecv addr) call
  where call = recv sock chunkSize

-- | Annotated 'timeout' combinator which given an 'IO' blocking operation,
-- returns it's value if it returns in less than 'time' microsenconds or
-- 'Nothing' if it takes longer than that.
--
-- Throws 'SysErr' on failure.
tryTimeout :: Int -> IO a -> IO (Maybe a)
tryTimeout time action = annotate OpTimeout call
  where call = timeout time action

-- | Annotated 'forkFinally' syscall which executes the provided monadic action,
-- on a new thread, ensuring that afterwards the handler is called, even on exceptions.
--
-- Works with any monad with 'MonadUnliftIO'.
--
-- Throws 'SysErr' on failure.
tryForkFinally :: MonadUnliftIO m => SockAddr -> m a -> (Either SomeException a -> m ()) -> m ()
tryForkFinally peer action handler = annotate (OpFork peer) call
  where call = void $ UIOC.forkFinally action handler

-- Listener -------------------------------------------------------------

-- | Returns a list of IP addresses and ports
-- compatile with TCP and the provided service.
--
-- Throws 'SysErr' on failure.
resolve :: ServiceName -> IO [AddrInfo]
resolve = tryGetAddrInfo
  defaultHints
    { addrFlags = [AI_PASSIVE]
    , addrSocketType = Stream
    }

-- | Given an 'AddrInfo' candidate, tries to acquire a compatible
-- socket, set it with the 'ReuseAddr' option so that the port can
-- be reused without timeouts, and bind it to said port.
--
-- Throws 'SysErr' on failure.
tryCandidate :: AddrInfo -> IO (Socket, AddrInfo)
tryCandidate addr = bracketOnError (tryOpenSock addr) close setupSock
  where
    setupSock sock = do
      trySetSockOpt sock ReuseAddr
      tryBindSock (addrAddress addr) sock
      pure (sock, addr)

-- | Given a list of compatible IP addresses and ports,
-- tries to find one for which 'tryCandidate' succeeds.
--
-- If an exception is thrown the next candidate is tried
-- unless the exception comes from 'setSocketOption'.
-- In that case the error is fatal and it is propagated.
--
-- Throws 'SysErr' on failure.
bindCandidate :: [AddrInfo] -> IO (Socket, AddrInfo)
bindCandidate []     = throwIO noCandidatesErr
bindCandidate [a]    = tryCandidate a
bindCandidate (a:as) = tryCandidate a `catch` \(err :: SysErr) -> do
  case seOper err of
    OpSockOpt -> throwIO err
    _         -> do
      emit Warn (show err)
      bindCandidate as

-- | Represents a listening socket.
type Listener = Socket

-- | Tries to acquire a listening socket
-- compatible with the specified service.
--
-- The 'backlog' parameter determines the maximum size
-- of the queue that stores pending connections.
--
-- Throws 'SysErr' on failure.
acquireListener :: ServiceName -> Int -> IO Listener
acquireListener service backlog = withSocketsDo $ do
  addrs        <- resolve service
  (sock, addr) <- bindCandidate addrs

  tryListenSock addr sock backlog

  pure sock

-- | Cleans up a 'Listener' by closing it's socket descriptor.
--
-- Throws 'SysErr' on failure.
releaseListener :: Listener -> IO ()
releaseListener = close

-- | Acquires a 'Listener' based on the app configuration,
-- runs an action which requires it, and ensures that the
-- resource is released afterwards, including on exceptions.
--
-- Throws 'SysErr' on failure.
withListener :: (Listener -> App a) -> App a
withListener action = do
  service <- asks (port . network . config)
  backlog <- asks (listenBacklog . network . config)
  UIO.bracket (liftIO $ acquireListener service backlog) (liftIO . releaseListener) action


-- Retry with Backoff ---------------------------------------------------

-- | When retrying an operation, encodes
-- what should be done in case of failure.
data RetryType
  = Immediate       -- ^ Retry immediately
  | Backoff         -- ^ Retry with backoff
  | Stop            -- ^ Stop retrying

-- | Given an 'IO' action, this combinator retries
-- the operation with exponential backoff with jitter.
--
-- The backoff time starts with 'delayMin' and increases
-- exponentially until reaching 'delayMax'. Additionally
-- a random jitter is introduced to reduce the thundering
-- herd problem.
--
-- The 'shouldRetry' parameter encodes whether the action
-- should be retried immediatly, with backoff, or stopped
-- entirely on failure, depending on the error emitted.
--
-- Throws 'SysErr' on failure.
retryBackoff :: Int -> Int -> (SysErr -> RetryType) -> IO a -> IO a
retryBackoff delayMin delayMax shouldRetry action = go delayMin
  where
    go delay = action `catch` \(err :: SysErr) ->
      case shouldRetry err of
        Immediate -> do                             -- If 'Immediatle' the delay is skipped
          emit Warn (show err)
          go delay
        Backoff -> do                               -- If 'Backoff' sleeps accordingly
          emit Warn (show err)
          jitter <- randomRIO (0, delay `div` 2)
          threadDelay (delay `div` 2 + jitter)
          go (min (delay*2) delayMax)
        Stop -> throwIO err                         -- If 'Stop' the exception is propagated


-- | Retry policy for accepting incoming connections.
shouldRetryAccept :: SysErr -> RetryType
shouldRetryAccept err =
  case classify err of
    Interrupted       -> Immediate          -- On interruption retry immediately
    ResourceVanished  -> Backoff            -- If the socket can't accept connections now wait
    ResourceExhausted -> Backoff            -- If the system doesn't have enough resources wait
    _                 -> Stop               -- Otherwise propagate the error

-- | Retry policy for receiving peer messages.
shouldRetryRecv :: SysErr -> RetryType
shouldRetryRecv err =
  case classify err of
    Interrupted       -> Immediate          -- On interruption retry immediately
    ResourceExhausted -> Backoff            -- If the system doesn't have enough resources wait
    _                 -> Stop               -- Otherwise propagate the error


-- Connection -----------------------------------------------------------

-- | Encodes a peer connection.
data Connection = Connection
  { connSock :: Socket              -- ^ Socket for message exchange
  , connPeer :: SockAddr            -- ^ Peer address and information
  , connBuff :: IORef ByteString    -- ^ Buffer for partially received queries
  }

instance Show Connection where
  show = show . connPeer

-- | Tries to acquire a client by waiting for
-- incoming connections on the listener socket.
--
-- The call to 'accept' is retried with the backoff
-- specified in the app configuration.
--
-- Throws 'SysErr' on failure.
acquireClient :: Listener -> App Connection
acquireClient listener =
  do
    delayMin     <- asks (backoffMin . network . config)
    delayMax     <- asks (backoffMax . network . config)

    (sock, addr) <- liftIO (retryAccept delayMin delayMax)
    buff         <- liftIO (newIORef BS.empty)

    pure $ Connection sock addr buff
  where
    retryAccept bmin bmax =
      retryBackoff bmin bmax
      shouldRetryAccept
      (tryAcceptClient listener)

-- | Cleans up a 'Connection' by closing it's socket descriptor.
--
-- Throws 'SysErr' on failure.
releaseClient :: Connection -> IO ()
releaseClient = close . connSock

-- | Encodes the action to perform if an
-- exception is thrown during a client session.
type ErrHandler = Connection -> SomeException -> App ()

-- | Acquires a 'Connection' by waiting for incoming clients
-- on the provided 'Listener', and runs an action that requires it
-- on a separate thread. Additionally, it ensures that the resource
-- is released afterwards, including on exceptions.
--
-- If an exception is thrown during a client session, the provided
-- error handler is called.
--
-- Throws 'SysErr' on failure.
forkClient :: Listener -> (Connection -> App ()) -> ErrHandler -> App ()
forkClient listener action handleExcp =
  do
    conn <- acquireClient listener
    tryForkFinally (connPeer conn) (action conn) (handler conn)
  where
    handler conn (Left err) = handleExcp conn err >> liftIO (releaseClient conn)
    handler conn (Right ()) = liftIO (releaseClient conn)


-- Sending --------------------------------------------------------------

-- | Given a serialized message, the function blocks
-- until it is sent entirely to the provided peer.
--
-- Throws 'SysErr' on failure.
sendClient :: Connection -> ByteString -> App ()
sendClient conn = liftIO . trySendAll (connPeer conn) (connSock conn)


-- Receiving ------------------------------------------------------------

-- | Encodes a failure when interacting with a client.
data ClientErr
  = TooLong     -- ^ Message is too long
  | Timeout     -- ^ Client timed out
  | Disconn     -- ^ Client disconnected

instance Show ClientErr where
  show err = case err of
    TooLong -> "query is too long"
    Timeout -> "autologout timeout expired"
    Disconn -> "client disconnected"

-- | Encodes the result of a receive call
data FetchResult
  = Chunk ByteString
  | PeerTimeout
  | PeerDisconn

-- | Fetches a chunk of data from the given 'Connection'.
--
-- The call to 'recv' is retried with backoff
-- and it times out based on the app configuration.
--
-- Throws 'SysErr' on failure.
fetchChunk :: Connection -> App FetchResult
fetchChunk conn = do
  chunkSize <- asks (readChunk   . network . config)
  delayMin  <- asks (backoffMin  . network . config)
  delayMax  <- asks (backoffMax  . network . config)
  time      <- asks (idleTimeout . network . config)

  liftIO $ UIO.handle handler $
    tryTimeout time (retryRecv delayMin delayMax chunkSize)
    <&> maybe PeerTimeout Chunk
  where
    retryRecv bmin bmax = retryBackoff bmin bmax shouldRetryRecv . tryRecv (connPeer conn) (connSock conn)
    handler err = case classify err of
      ResourceVanished -> pure PeerDisconn
      _                -> throwIO err

-- | Strips the terminating CRLF of a message.
stripCRLF :: ByteString -> ByteString
stripCRLF = BS.drop 2

-- | Encodes the result of processing
-- a message received from a client.
data SplitResult
  = Complete ByteString ByteString      -- ^ Complete CRLF terminated message followed by the remaining bytes
  | Incomplete                          -- ^ Message without terminating CRLF
  | Overflow (Maybe ByteString)         -- ^ Messaging which surpasses the maximum length followed by remaining bytes if incomplete

-- | Attempts to extract a complete message from a chunk
-- of bytes received from the peer, and encodes it on a 'SplitResult'.
splitLine :: ByteString -> SplitResult
splitLine buff =
  case BS.breakSubstring "\r\n" buff of
    (line, rest)
      | BS.length line + 2 > queryMaxLen -> evalOverflow rest
      | BS.null rest                     -> Incomplete
      | otherwise                        -> Complete line (stripCRLF rest)
  where
    evalOverflow rest
      | BS.null rest = Overflow Nothing
      | otherwise    = Overflow $ Just (stripCRLF rest)

-- | Discards all data received from the client until a CRLF is received.
--
-- This function is called when the client surpasses the maximum message length
-- without sending a CRLF terminator. As the message is invalid, all data received
-- is drained until the terminator is received.
--
-- When the terminator is received, the remaining bytes are saved on the
-- 'Connection' buffer and a 'TooLong' error is returned.
--
-- Throws 'SysErr' on failure.
drainLine :: Connection -> App (Either ClientErr ByteString)
drainLine conn = fetchChunk conn >>= \case
  PeerDisconn -> pure (Left Disconn)
  PeerTimeout -> pure (Left Timeout)
  Chunk bytes ->
    case BS.breakSubstring "\r\n" bytes of
      (_, rest) | not (BS.null rest) -> liftIO $ writeIORef (connBuff conn) (stripCRLF rest) $> Left TooLong
      _                              -> drainLine conn

-- | Receives and returns a single CRLF terminated line from the provided client.
--
-- Buffered data is consumed first, fetching additional chunks from the socket
-- as needed. Complete messages are returned without the trailing CRLF, while
-- unread bytes are preserved for subsequent calls.
--
-- On failure, it returns 'Left' with 'TooLong' for messages exceeding the maximum
-- allowed length, 'Disconn' if the peer disconnects, or 'Timeout' on receive timeout.
--
-- Throws 'SysErr' on failure.
recvClient :: Connection -> App (Either ClientErr ByteString)
recvClient conn = liftIO (readIORef (connBuff conn)) >>= go
  where
    go buff = do
      case splitLine buff of
        Overflow Nothing     -> drainLine conn
        Overflow (Just rest) -> UIO.writeIORef (connBuff conn) rest $> Left TooLong
        Complete line rest   -> UIO.writeIORef (connBuff conn) rest $> Right line
        Incomplete -> fetchChunk conn >>= \case
          PeerDisconn -> pure (Left Disconn)
          PeerTimeout -> pure (Left Timeout)
          Chunk bytes -> go (buff <> bytes)
