module Error
  ( SysErr(..)
  , Oper(..)
  , annotate
  , classify
  , noCandidatesErr
  , corruptMailErr
  ) where

import Control.Exception
  ( IOException
  , throwIO
  , catch
  , Exception (displayException)
  )

import System.IO.Error (IOErrorType, ioeGetErrorType)
import Network.Socket (SockAddr)

data Oper
  = OpAddrInfo
  | OpOpenSock SockAddr
  | OpSockOpt
  | OpBind SockAddr
  | OpListen SockAddr
  | OpAccept
  | OpTimeout
  | OpSend SockAddr
  | OpRecv SockAddr
  | OpDirExist FilePath
  | OpOpen FilePath
  | OpClose FilePath
  | OpUnlink FilePath
  | OpListDir FilePath
  | OpStatFile FilePath
  | OpMove FilePath FilePath
  | OpRead
  | OpUser

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
    OpDirExist path -> "failed when checking existance of directory " ++ path
    OpOpen path     -> "failed when opening file " ++ path
    OpClose path    -> "failed when closing file " ++ path
    OpUnlink path   -> "failed when unlinking file " ++ path
    OpListDir path  -> "failed when listing directory " ++ path
    OpStatFile path -> "failed to stat file " ++ path
    OpMove src dst  -> "failed to move file from " ++ src ++ " to " ++ dst
    OpRead          -> "failed when reading file"
    OpUser          -> ""

data SysErr = SysErr
  { seOper :: Oper
  , seExcp :: IOException
  }

instance Show SysErr where
  show (SysErr op err) =
    show op ++ " -> " ++ displayException err

instance Exception SysErr

annotate :: Oper -> IO a -> IO a
annotate op action =
  action `catch` (throwIO . SysErr op)

classify :: SysErr -> IOErrorType
classify err = ioeGetErrorType (seExcp err)

noCandidatesErr :: SysErr
noCandidatesErr = SysErr
  { seOper = OpUser
  , seExcp = userError "no addrinfo candidates found"
  }

corruptMailErr :: SysErr
corruptMailErr = SysErr
  { seOper = OpUser
  , seExcp = userError "inconsistent mail format found"
  }
