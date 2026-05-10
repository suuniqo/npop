{-# LANGUAGE ScopedTypeVariables #-}

module Storage
  ( StorageErr (..)
  , Message (..)
  , Flag (..)
  , UID
  , Username
  , Lock
  , withLock
  , userValidate
  , fetchMailbox
  , updateMailbox
  ) where

import Control.Exception (bracket, catch, throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isAlphaNum)
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
  , SysErr, annotate, classify
  )

-- Data

data Flag
  = Draft
  | Passed
  | Replied
  | Seen
  | Trashed
  deriving (Eq, Ord)

newtype UID = UID { unUID :: String }

data Message = Message
  { msgPath  :: !FilePath
  , msgSize  :: !Integer
  , msgTime  :: !POSIXTime
  , msgFlags :: !(Set Flag)
  , msgUid   :: !UID
  }

data Lock = Lock
  { lockPath :: !FilePath
  , lockFd   :: !Fd
  }

newtype Username = Username String

data StorageErr
  = UserLocked
  | InvalidUser
  | NoSuchUser
  | CorruptMail

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

storageRoot :: FilePath
storageRoot = "/mail"

lockName :: FilePath
lockName = ".lock"

curName :: FilePath
curName = "cur"

newName :: FilePath
newName = "new"

mailSep :: FilePath
mailSep = ":2"

userRoot :: Username -> FilePath
userRoot (Username user) = storageRoot </> user

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

buildFlags :: String -> Maybe (Set Flag)
buildFlags info = case stripPrefix mailSep info of
  Nothing -> Just Set.empty
  Just fs -> Set.fromList <$> mapM flagFromChar fs

buildTime :: String -> Maybe POSIXTime
buildTime = readMaybe . takeWhile (/= '.')

buildUid :: String -> Maybe UID
buildUid = Just . UID

buildMessage :: (FilePath, FileOffset) -> Maybe Message
buildMessage (path, size) = do
  let (base, info) = break (== ':') (takeFileName path)

  flags <- buildFlags info
  time  <- buildTime base
  uid   <- buildUid base

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

userValidate :: ByteString -> Either StorageErr Username
userValidate user
  | BS.all isAlphaNum user = Right $ Username (BS.unpack user)
  | otherwise              = Left InvalidUser

withLock :: Username -> (Lock -> IO a) -> IO (Either StorageErr a)
withLock user action =
  (Right <$> bracket (acquireLock (userLockPath user)) releaseLock action)
    `catch` \(err :: SysErr) ->
      case classify err of
        AlreadyExists -> pure $ Left UserLocked
        NoSuchThing   -> pure $ Left NoSuchUser
        _             -> throwIO err

fetchMailbox :: Lock -> Username -> IO (Either StorageErr (Seq Message))
fetchMailbox _ user = do
  maybeMsgs <- mapM buildMessage <$> maildirFiles user

  pure $ case maybeMsgs of
    Nothing   -> Left CorruptMail
    Just msgs -> Right (Seq.fromList msgs)

updateMailbox :: Lock -> Username -> [Message] -> [Message] -> IO ()
updateMailbox _ user trash keep = do
  mapM_ (moveMessage (userRoot user)) keep
  mapM_ deleMessage trash
