{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Server
  ( Listener
  , withListener
  , Connection (..)
  , acquireClient
  , withClient
  , withClient'
  , ClientErr (..)
  , sendClient
  , recvClient
  ) where

import GHC.IO.Exception (IOErrorType(Interrupted, ResourceVanished, ResourceExhausted, ResourceBusy))

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

import Control.Concurrent (threadDelay)

import Control.Exception (throwIO, bracketOnError, catch, bracket)

import System.Random

import Error
  ( noCandidatesErr
  , annotate
  , classify
  , Oper (..)
  , SysErr (seOper)
  )
import Log (emit, Severity (Warn))
import Network.Socket.ByteString (sendAll, recv)
import Data.ByteString (ByteString)
import Config (listenBacklog, maxSizeQuery, backoffMin, backoffMax, idleTimeout, readChunk)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import qualified Data.ByteString as BS
import System.Timeout (timeout)

tryGetAddrInfo :: AddrInfo -> ServiceName -> IO [AddrInfo]
tryGetAddrInfo hints port = annotate OpAddrInfo call
  where call = getAddrInfo (Just hints) Nothing (Just port)

tryOpenSock :: AddrInfo -> IO Socket
tryOpenSock addr = annotate (OpOpenSock $ addrAddress addr) call
  where call = openSocket addr

trySetSockOpt :: Socket -> SocketOption -> IO ()
trySetSockOpt sock opt = annotate OpSockOpt call
  where call = setSocketOption sock opt 1

tryBindSock :: AddrInfo -> Socket -> IO ()
tryBindSock addr sock = annotate (OpBind $ addrAddress addr) call
  where call = bind sock (addrAddress addr)

tryListenSock :: AddrInfo -> Socket -> Int -> IO ()
tryListenSock addr sock queue = annotate (OpListen $ addrAddress addr) call
  where call = listen sock queue

tryAcceptClient :: Socket -> IO (Socket, SockAddr)
tryAcceptClient listener = annotate OpAccept call
  where call = accept listener

trySendAll :: SockAddr -> Socket -> ByteString -> IO ()
trySendAll addr sock msg = annotate (OpSend addr) call
  where call = sendAll sock msg

tryRecv :: SockAddr -> Socket -> IO ByteString
tryRecv addr sock = annotate (OpRecv addr) call
  where call = recv sock readChunk 

tryTimeout :: Int -> IO a -> IO (Maybe a)
tryTimeout time action = annotate OpTimeout call
  where call = timeout time action

resolve :: ServiceName -> IO [AddrInfo]
resolve = tryGetAddrInfo
  defaultHints
    { addrFlags = [AI_PASSIVE]
    , addrSocketType = Stream
    }

tryCandidate :: AddrInfo -> IO (Socket, AddrInfo)
tryCandidate addr = bracketOnError (tryOpenSock addr) close setupSock
  where
    setupSock sock = do
      trySetSockOpt sock ReuseAddr
      tryBindSock addr sock
      pure (sock, addr)

bindCandidate :: [AddrInfo] -> IO (Socket, AddrInfo) 
bindCandidate []     = throwIO noCandidatesErr
bindCandidate [a]    = tryCandidate a
bindCandidate (a:as) = tryCandidate a `catch` \(err :: SysErr) -> do
  case seOper err of
    OpSockOpt -> throwIO err
    _         -> do
      emit Warn err
      bindCandidate as

type Listener = Socket

acquireListener :: ServiceName -> IO Listener
acquireListener host = withSocketsDo $ do
  addrs <- resolve host
  (sock, addr) <- bindCandidate addrs

  tryListenSock addr sock listenBacklog

  pure sock

releaseListener :: Listener -> IO ()
releaseListener = close

withListener :: ServiceName -> (Listener -> IO a) -> IO a
withListener port = bracket (acquireListener port) releaseListener

data Connection = Connection
  { connSock :: Socket
  , connPeer :: SockAddr
  , connBuff :: IORef ByteString
  }

data RetryType
  = Immediate
  | Backoff
  | Stop

retryBackoff :: Int -> Int -> (SysErr -> RetryType) -> IO a -> IO a
retryBackoff minDelay maxDelay shouldRetry action = go (min minDelay maxDelay)
  where
    go delay = action `catch` \(err :: SysErr) ->
      case shouldRetry err of
        Stop -> throwIO err
        Immediate -> do
          emit Warn err
          go delay
        Backoff -> do
          emit Warn err
          jitter <- randomRIO (0, delay `div` 2)
          threadDelay (delay `div` 2 + jitter)
          go (min (delay*2) maxDelay)

shouldRetryAccept :: SysErr -> RetryType
shouldRetryAccept err =
  case classify err of
    Interrupted        -> Immediate
    ResourceVanished   -> Backoff
    ResourceExhausted  -> Backoff
    _                  -> Stop

acquireClient :: Listener -> IO Connection
acquireClient listener = do
  (sock, addr) <- retryAccept
  buff <- newIORef BS.empty
  pure $ Connection sock addr buff
  where
    retryAccept = retryBackoff
      backoffMin
      backoffMax
      shouldRetryAccept
      (tryAcceptClient listener)

releaseClient :: Connection -> IO ()
releaseClient = close . connSock

withClient :: Listener -> (Connection -> IO a) -> IO a
withClient listener = bracket (acquireClient listener) releaseClient

withClient' :: Connection -> (Connection -> IO a) -> IO a
withClient' conn = bracket (pure conn) releaseClient

sendClient :: Connection -> ByteString -> IO ()
sendClient conn = trySendAll (connPeer conn) (connSock conn)

shouldRetryRecv :: SysErr -> RetryType
shouldRetryRecv err =
  case classify err of
    Interrupted -> Immediate
    _           -> Stop

data ClientErr
  = TooLong
  | Timeout
  | Disconn

instance Show ClientErr where
  show err = case err of
    TooLong -> "query is too long"
    Timeout -> "autologout timeout expired"
    Disconn -> "client disconnected"

recvClient :: Connection -> IO (Either ClientErr ByteString)
recvClient conn = do
  buff <- readIORef $ connBuff conn
  go buff
  where
    retryRecv = retryBackoff
      backoffMin
      backoffMax
      shouldRetryRecv
      (tryRecv (connPeer conn) (connSock conn))
    go buff = case BS.breakSubstring "\r\n" buff of
      (line, rest)
        | BS.length line + 2 > maxSizeQuery ->
            pure (Left TooLong)
        | not (BS.null rest) -> do
            writeIORef (connBuff conn) (BS.drop 2 rest)
            pure (Right line)
        | otherwise -> do
            maybeChunk <- tryTimeout idleTimeout retryRecv
              `catch` \(err :: SysErr) -> case classify err of
                ResourceBusy  -> pure (Just BS.empty)
                _             -> throwIO err
            case maybeChunk of
              Nothing -> pure (Left Timeout)
              Just chunk -> do
                if BS.null chunk
                then pure (Left Disconn)
                else go (buff <> chunk)
