{- |
Module      : Log
Description : Server Logging
Portability : POSIX

Logging module for structured log output to stderr.
-}
module Log
  ( Severity (..)
  , emit
  ) where


-- Imports --------------------------------------------------------------

import Control.Concurrent (ThreadId, myThreadId)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (intercalate)
import System.IO (hPrint, stderr)

import Constant (progName)


-- Types ----------------------------------------------------------------

-- | Severity level of a log entry.
data Severity
  = Info        -- ^ Normal operational messages
  | Warn        -- ^ Non-fatal unexpected conditions
  | Fail        -- ^ Unrecoverable errors

instance Show Severity where
  show Info = "info"
  show Warn = "warn"
  show Fail = "fail"

-- | Internal representation of a log,
-- including severity, thread id and message.
data LogEntry = LogEntry Severity String ThreadId

instance Show LogEntry where
  show (LogEntry sever msg thid) = intercalate ": " [progName, show thid, show sever, msg]


-- Operations -----------------------------------------------------------

-- | Writes a 'LogEntry' to stderr.
emitEntry :: LogEntry -> IO ()
emitEntry = hPrint stderr

-- | Emits a log entry to stderr with the included
-- severity and message. Works with any monad with 'MonadIO'.
--
-- Output format: @progName: ThreadId N: severity: message@
--
-- Examples:
--
-- > emit Info "normal"
-- > emit Warn "unexpected"
-- > emit Fail "unrecoverable"
emit :: MonadIO m => Severity -> String -> m ()
emit sever msg = liftIO $ entry >>= emitEntry
  where entry = LogEntry sever msg <$> myThreadId
