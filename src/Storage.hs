{-# LANGUAGE ScopedTypeVariables #-}

module Storage
  ( StorageErr (..)
  , Message (..)
  , Flag (..)
  , Lock
  , withLock
  , fetchMailbox
  , updateMailbox
  ) where

import Control.Exception (bracket, catch, throwIO)
import Data.List (stripPrefix)
import Data.Set (Set)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.IO.Exception (IOErrorType (AlreadyExists, NoSuchThing))
import System.Directory (listDirectory, renameFile)
import System.FilePath
import System.Posix
  ( Fd, FileOffset, FileStatus
  , OpenFileFlags (creat, exclusive)
  , OpenMode (ReadWrite)
  , closeFd, defaultFileFlags, fileSize
  , getFileStatus, isRegularFile
  , openFd, removeLink
  )
import Text.Read (readMaybe)

import Error
  ( Oper (OpClose, OpListDir, OpOpen, OpStatFile, OpUnlink, OpMove)
  , SysErr, annotate, classify, corruptMailErr
  )

import Config (storageRoot)
import Types (Username (unUser), UID (unUID), readUID)

-- Data

data Flag
  = Draft
  | Passed
  | Replied
  | Seen
  | Trashed
  deriving (Show, Eq, Ord)

data Message = Message
  { msgPath  :: !FilePath
  , msgSize  :: !Integer
  , msgTime  :: !POSIXTime
  , msgFlags :: !(Set Flag)
  , msgUid   :: !UID
  }
  deriving Show

data Lock = Lock
  { lockPath :: !FilePath
  , lockFd   :: !Fd
  }
  deriving Show

data StorageErr
  = UserLocked
  | NoSuchUser

instance Show StorageErr where
  show err = case err of
    UserLocked -> "mailbox in use"
    NoSuchUser -> "invalid credentials"

-- Syscalls

tryOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO Fd
tryOpen path mode flags = annotate (OpOpen path) call
  where call = openFd path mode flags

tryClose :: FilePath -> Fd -> IO ()
tryClose path fd = annotate (OpClose path) call
  where call = closeFd fd

tryOpenExcl :: FilePath -> IO Fd
tryOpenExcl path = tryOpen path ReadWrite flags
  where
    flags = defaultFileFlags
      { creat = Just 0o600
      , exclusive = True
      }

tryUnlink :: FilePath -> IO ()
tryUnlink path = annotate (OpUnlink path) call
  where call = removeLink path

tryListDir :: FilePath -> IO [FilePath]
tryListDir path = annotate (OpListDir path) call
  where call = map (path </>) <$> listDirectory path

tryStatFile :: FilePath -> IO FileStatus
tryStatFile path = annotate (OpStatFile path) call
  where call = getFileStatus path

tryMove :: FilePath -> FilePath -> IO ()
tryMove src dst = annotate (OpMove src dst) call
  where call = renameFile src dst

-- Layout

lockName :: FilePath
lockName = ".lock"

curName :: FilePath
curName = "cur"

newName :: FilePath
newName = "new"

mailSep :: FilePath
mailSep = ":2"

userRoot :: Username -> FilePath
userRoot user = storageRoot </> unUser user

userLockPath :: Username -> FilePath
userLockPath user = userRoot user </> lockName

-- Lock

releaseLock :: Lock -> IO ()
releaseLock lock = do
  tryClose (lockPath lock) (lockFd lock)
  tryUnlink (lockPath lock)

acquireLock :: FilePath -> IO Lock
acquireLock path = Lock path <$> tryOpenExcl path

-- Filesystem

fileWithStat :: FilePath -> IO [(FilePath, FileStatus)]
fileWithStat path = do
  files <- tryListDir path
  mapM (\fp -> (,) fp <$> tryStatFile fp) files

fileWithSizes :: FilePath -> IO [(FilePath, FileOffset)]
fileWithSizes path = do
  files <- fileWithStat path
  pure [(fp, fileSize st) | (fp, st) <- files, isRegularFile st]

maildirFiles :: Username -> IO [(FilePath, FileOffset)]
maildirFiles user = do
  let userPath = userRoot user

  curFiles <- fileWithSizes (userPath </> curName)
  newFiles <- fileWithSizes (userPath </> newName)

  pure (curFiles <> newFiles)

-- Maildir Parsing

flagFromChar :: Char -> Maybe Flag
flagFromChar c = case c of
  'S' -> Just Seen
  'R' -> Just Replied
  'T' -> Just Trashed
  'D' -> Just Draft
  'P' -> Just Passed
  _   -> Nothing

readFlags :: String -> Maybe (Set Flag)
readFlags info = case stripPrefix mailSep info of
  Nothing -> Just Set.empty
  Just fs -> Set.fromList <$> mapM flagFromChar fs

readTime :: String -> Maybe POSIXTime
readTime = (toTime <$>) . readMaybe . takeWhile (/= '.')
  where toTime = fromIntegral :: Integer -> POSIXTime

buildMessage :: (FilePath, FileOffset) -> Maybe Message
buildMessage (path, size) = do
  let (base, info) = break (== ':') (takeFileName path)

  flags <- readFlags info
  time  <- readTime base
  uid   <- readUID base

  pure $ Message path (fromIntegral size) time flags uid

-- Maildir Dumping

flagToChar :: Flag -> Char
flagToChar flag = case flag of
  Seen    -> 'S'
  Replied -> 'R'
  Trashed -> 'T'
  Draft   -> 'D'
  Passed  -> 'P'

flagged :: Flag -> Message -> Bool
flagged flag = Set.member flag . msgFlags

infoMessage :: Message -> FilePath
infoMessage = map flagToChar . Set.toList . msgFlags

dirMessage :: Message -> FilePath
dirMessage msg
  | flagged Seen msg = curName
  | otherwise        = newName

deleMessage :: Message -> IO ()
deleMessage = tryUnlink . msgPath

moveMessage :: FilePath -> Message -> IO ()
moveMessage root msg = tryMove src (root </> dir </> dst)
  where
    src = msgPath msg
    dir = dirMessage msg
    dst = (unUID . msgUid) msg <> mailSep <> infoMessage msg

-- Methods

withLock :: Username -> (Lock -> IO a) -> IO (Either StorageErr a)
withLock user action = do
  result <- (Right <$> acquireLock (userLockPath user))
    `catch` \(err :: SysErr) -> case classify err of
      AlreadyExists -> pure $ Left UserLocked
      NoSuchThing   -> pure $ Left NoSuchUser
      _             -> throwIO err

  case result of
    Left e     -> pure $ Left e
    Right lock -> Right <$> bracket (pure lock) releaseLock action

fetchMailbox :: Lock -> Username -> IO (Seq Message)
fetchMailbox _ user = do
  maybeMsgs <- mapM buildMessage <$> maildirFiles user

  case maybeMsgs of
    Nothing   -> throwIO corruptMailErr
    Just msgs -> pure $ Seq.fromList msgs

updateMailbox :: Lock -> Username -> [Message] -> [Message] -> IO ()
updateMailbox _ user trash seen = do
  mapM_ (moveMessage (userRoot user)) seen
  mapM_ deleMessage trash
