{- |
Module      : Error
Description : System Exception Handling
Portability : POSIX

To improve the messages of lower level syscalls and add more
context to errors, they are wrapped with the 'SysErr' type.
-}
module Error
  ( -- * Types
    SysErr (..)
  , Oper (..)
    -- * Operations
  , classify
  , annotate
    -- * Custom Errors
  , noCandidatesErr
  , corruptMailErr
  ) where


-- Imports --------------------------------------------------------------

import Control.Exception
  ( IOException
  , Exception (displayException)
  )
import Network.Socket (SockAddr)
import System.IO.Error (IOErrorType, ioeGetErrorType)
import qualified UnliftIO as UIO
import UnliftIO (MonadUnliftIO)


-- Types ----------------------------------------------------------------

-- | Identifies which syscall or operation produced a 'SysErr',
-- carrying enough context to produce a useful error message.
data Oper
  = OpAddrInfo                  -- ^ 'getAddrInfo' name resolution
  | OpOpenSock   SockAddr       -- ^ 'openSocket' for the given address
  | OpSockOpt                   -- ^ 'setSocketOption'
  | OpBind       SockAddr       -- ^ 'bind' to the given address
  | OpListen     SockAddr       -- ^ 'listen' on the given address
  | OpAccept                    -- ^ 'accept' incoming connection
  | OpTimeout                   -- ^ 'timeout' on IO operation
  | OpFork       SockAddr       -- ^ 'fork' a connection on separate thread
  | OpSend       SockAddr       -- ^ 'sendAll' to the given peer
  | OpRecv       SockAddr       -- ^ 'recv' from the given peer
  | OpOpen                      -- ^ 'openFd' on the given path
  | OpClose      FilePath       -- ^ 'closeFd' on the given path
  | OpUnlink                    -- ^ 'removeLink' on the given path
  | OpListDir                   -- ^ 'listDirectory'
  | OpStatFile                  -- ^ 'getFileStatus' on the given path
  | OpMove                      -- ^ 'renameFile' from first path to second
  | OpRead                      -- ^ 'readFile'
  | OpUser                      -- ^ User-defined exception, not a syscall

instance Show Oper where
  show op = case op of
    OpAddrInfo      -> "failed to resolve any addresses"
    OpOpenSock addr -> "failed to open socket on " ++ show addr
    OpSockOpt       -> "failed to set ReuseAddr flag"
    OpBind addr     -> "failed to bind on " ++ show addr
    OpListen addr   -> "failed to listen on " ++ show addr
    OpAccept        -> "failed to accept"
    OpRecv addr     -> "failed when receiving query on " ++ show addr
    OpSend addr     -> "failed when sending response on " ++ show addr
    OpTimeout       -> "failed to perform timeout"
    OpFork peer     -> "failed to fork on a new thread connection " ++ show peer
    OpOpen          -> "failed when opening file"
    OpClose path    -> "failed when closing file " ++ show path
    OpUnlink        -> "failed when unlinking file"
    OpListDir       -> "failed when listing directory"
    OpStatFile      -> "failed to stat file"
    OpMove          -> "failed to move file"
    OpRead          -> "failed when reading file"
    OpUser          -> ""

-- | System exception wrapper, containing both the 'IOException'
-- which caused it and an 'Oper' which identifies the syscall
-- and carries additional context.
data SysErr = SysErr
  { seOper :: Oper          -- ^ Operation which was attempted
  , seExcp :: IOException   -- ^ Underlying thrown exception
  }

instance Show SysErr where
  show (SysErr op err) =
    show op ++ ": " ++ displayException err

instance Exception SysErr


-- Operations -----------------------------------------------------------

-- | Extracts the 'IOErrorType' from a 'SysErr',
-- which further clarifies the origin
classify :: SysErr -> IOErrorType
classify err = ioeGetErrorType (seExcp err)

-- | Wraps an 'IO' action so that any 'IOException'
-- thrown is correctly caught and contextualized with 'SysErr'.
--
-- Exceptions are rethrown rather than converted to 'Either SysErr a'
-- only when they couldn't have been caused by a client. In other words,
-- this function should only be used when an error means an internal failure.
--
-- Works with any Monda with 'MonadUnliftIO'.
--
-- Example:
--
-- > annotate (OpRecv addr) (recv sock 4096)
annotate :: MonadUnliftIO m => Oper -> m a -> m a
annotate op action = action `UIO.catch` (UIO.throwIO . SysErr op)


-- Custom Errors --------------------------------------------------------

-- | Thrown when 'getAddrInfo' returns an empty candidate list,
-- so building a listener socket is not possible.
noCandidatesErr :: SysErr
noCandidatesErr = SysErr
  { seOper = OpUser
  , seExcp = userError "no addrinfo candidates found"
  }

-- | Thrown when the Maildir layout is incorrect or malformed.
--
-- For example, when a user appears in the shadow file but
-- doesn't have a mailbox or is missing one of its directories.
corruptMailErr :: SysErr
corruptMailErr = SysErr
  { seOper = OpUser
  , seExcp = userError "inconsistent mail format found"
  }
