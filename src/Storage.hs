{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module      : Storage
Description : Filesystem storage operations
Portability : POSIX

Manages the storage of mail in the filesystem,
offering high level function to load and update maildrops.
-}
module Storage
  ( -- * Types
    StorageErr (..)
    -- * Locking
  , Lock
  , withLock
    -- * Mailbox
  , fetchMailbox
  , updateMailbox
  ) where


-- Imports --------------------------------------------------------------

import Control.Exception (catch, throwIO)
import Control.Monad.RWS (asks, MonadIO (liftIO))
import Data.List (stripPrefix)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.IO.Exception (IOErrorType (AlreadyExists))
import System.Directory (listDirectory, renameFile)
import System.FilePath
import System.Posix
  ( Fd
  , FileStatus
  , OpenFileFlags (creat, exclusive)
  , OpenMode (ReadWrite)
  , closeFd
  , defaultFileFlags
  , fileSize
  , getFileStatus
  , isRegularFile
  , openFd
  , removeLink
  )
import Text.Read (readMaybe)
import qualified UnliftIO as UIO

import App (App, AppEnv (config))
import Config (StorageConfig(mailRoot, lockName), Config (storage))
import Constant (mailSep, curName, newName)
import Error
  ( Oper (OpClose, OpListDir, OpOpen, OpStatFile, OpUnlink, OpMove)
  , SysErr
  , annotate
  , classify
  , corruptMailErr
  )
import Types
  ( Username (unUser)
  , UID (unUID)
  , readUID
  , Flag (..)
  , Message (..)
  , hasFlag
  )


-- Types ----------------------------------------------------------------

-- | Encodes a failure when loading or storing messages.
data StorageErr
  = UserLocked          -- ^ User's mailbox is in use

instance Show StorageErr where
  show err = case err of
    UserLocked -> "mailbox in use"


-- Syscalls -------------------------------------------------------------

-- | Annotated 'openFd' syscall which tries to open
-- the provided file and returns it's file descriptor.
--
-- Throws 'SysErr' on failure.
tryOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO Fd
tryOpen path mode flags = annotate OpOpen call
  where call = openFd path mode flags

-- | Annotated 'closeFd' syscall which tries 
-- to close the provided file descriptor.
--
-- Throws 'SysErr' on failure.
tryClose :: FilePath -> Fd -> IO ()
tryClose path fd = annotate (OpClose path) call
  where call = closeFd fd

-- | Annotated 'removeLink' syscall which tries
-- to remove the provided file descriptor.
--
-- Throws 'SysErr' on failure.
tryUnlink :: FilePath -> IO ()
tryUnlink path = annotate OpUnlink call
  where call = removeLink path

-- | Annotated 'listDirectory' syscall which tries
-- to fetch the 'FilePath' of all files on a directory.
--
-- Throws 'SysErr' on failure.
tryListDir :: FilePath -> IO [FilePath]
tryListDir path = annotate OpListDir call
  where call = map (path </>) <$> listDirectory path

-- | Annotated 'getFileStatus' syscall which tries
-- to stat the provided file and returns it's information.
--
-- Throws 'SysErr' on failure.
tryStatFile :: FilePath -> IO FileStatus
tryStatFile path = annotate OpStatFile call
  where call = getFileStatus path

-- | Annotated 'renameFile' syscall which, given a file,
-- tries to move it to the provided destination.
--
-- Throws 'SysErr' on failure.
tryMove :: FilePath -> FilePath -> IO ()
tryMove src dst = annotate OpMove call
  where call = renameFile src dst


-- Maildir Layout -------------------------------------------------------

-- | Returns the mailbox root directory corresponding to
-- the provided user, based on the app configuration.
userMailbox :: Username -> App FilePath
userMailbox user = do
  root <- asks (mailRoot . storage . config)
  pure (root </> unUser user)

-- | Returns the mailbox lock path corresponding to
-- the provided user, based on the app configuration.
userLockPath :: Username -> App FilePath
userLockPath user = do
  name    <- asks (lockName . storage . config)
  mailbox <- userMailbox user
  pure (mailbox </> name)


-- Locking --------------------------------------------------------------

-- Encodes a mailbox lock.
data Lock = Lock
  { lockPath :: !FilePath       -- ^ Filepath of the lock
  , lockFd   :: !Fd             -- ^ File descriptor of the lock
  } deriving Show

-- | Tries to create and open the provided file with the O_EXCL flag,
-- which enforces that the file is opened by a single thread at a time.
--
-- If the file can be opened exclusively, returns it's file
-- descriptor. Otherwise, 'Nothing' is returned.
--
-- Throws 'SysErr' on failure.
tryOpenExcl :: FilePath -> IO (Maybe Fd)
tryOpenExcl path =
  let
    flags = defaultFileFlags { creat = Just 0o600 , exclusive = True }
  in
    (Just <$> tryOpen path ReadWrite flags)
      `catch` \(err :: SysErr) -> case classify err of
        AlreadyExists -> pure Nothing
        _             -> throwIO err

-- | Releases the provided lock file by first closing
-- the exclusive file descriptor and then removing the file.
--
-- Throws 'SysErr' on failure.
releaseLock :: Lock -> IO ()
releaseLock lock = do
  tryClose (lockPath lock) (lockFd lock)
  tryUnlink (lockPath lock)

-- | Acquires an exclusive file descriptor by creating and
-- opening exclusively a file in the provided path.
--
-- Throws 'SysErr' on failure.
acquireLock :: FilePath -> IO (Either StorageErr Lock)
acquireLock path =
  maybe (Left UserLocked) (Right . Lock path) <$> tryOpenExcl path

-- | Acquires a 'Lock' for the provided user's mailbox,
-- runs an action which requires it, and ensures that the
-- resource is released afterwards, including on exceptions.
--
-- If the mailbox is already in use it returns 'Left' with 'UserLocked'.
--
-- Throws 'SysErr' on failure.
withLock :: Username -> (Lock -> App a) -> App (Either StorageErr a)
withLock user action = do
  path   <- userLockPath user
  result <- liftIO $ acquireLock path

  case result of
    Left e     -> pure $ Left e
    Right lock -> Right <$> UIO.bracket (pure lock) (liftIO . releaseLock) action


-- File Operations ------------------------------------------------------

-- | Given the path of a directory, returns a list
-- with the paths of all the files it contains along
-- with their information.
--
-- Throws 'SysErr' on failure.
fileWithStat :: FilePath -> IO [(FilePath, FileStatus)]
fileWithStat path = do
  files <- tryListDir path
  mapM (\fp -> (,) fp <$> tryStatFile fp) files

-- | Given the path of a directory, returns a list
-- with the paths of all the regular files it contains
-- along with their size in bytes.
--
-- Throws 'SysErr' on failure.
fileWithSizes :: FilePath -> IO [(FilePath, Integer)]
fileWithSizes path = do
  files <- fileWithStat path
  pure [(fp, fromIntegral $ fileSize st) | (fp, st) <- files, isRegularFile st]

-- | Returns a list with the paths of all the mail files
-- of a given user along with their size in bytes.
--
-- Throws 'SysErr' on failure.
maildirFiles :: Username -> App [(FilePath, Integer)]
maildirFiles user = do
  userPath <- userMailbox user

  curFiles <- liftIO $ fileWithSizes (userPath </> curName)
  newFiles <- liftIO $ fileWithSizes (userPath </> newName)

  pure (curFiles <> newFiles)


-- Message Parsing ------------------------------------------------------

-- | Maps a character to the correspondig
-- Maildir flag. Returns 'Nothing' if invalid.
flagFromChar :: Char -> Maybe Flag
flagFromChar c = case c of
  'S' -> Just Seen
  'R' -> Just Replied
  'T' -> Just Trashed
  'D' -> Just Draft
  'P' -> Just Passed
  _   -> Nothing

-- | Parses a chain of characters into a 'Set'
-- with the corresponding Maildir flags.
-- Returns 'Nothing' if any character is invalid.
readFlags :: String -> Maybe (Set Flag)
readFlags info = case stripPrefix mailSep info of
  Nothing -> Just Set.empty
  Just fs -> Set.fromList <$> mapM flagFromChar fs

-- | Parses the 'POSIXTime' from a Maildir
-- file name. Returns 'Nothing' on failure.
readTime :: String -> Maybe POSIXTime
readTime = (toTime <$>) . readMaybe . takeWhile (/= '.')
  where toTime = fromIntegral :: Integer -> POSIXTime

-- | Parses a message from it's file name and size.
--
-- Maildir messages are stored in the format:
-- @<timestamp>.<pid>.<hostname>[:2<flags>]@
--
-- Therefore, when building a message, the time
-- is extracted from the first field, the flags
-- from the optional last field and finally the
-- @<timestamp>.<pid>.<hostname>@ prefix is used
-- as the message UID.
--
-- Returns 'Nothing' if any field can't be parsed.
buildMessage :: (FilePath, Integer) -> Maybe Message
buildMessage (path, size) =
  let
    (base, info) = break (== ':') (takeFileName path)
  in 
    Message path size
      <$> readTime  base
      <*> readFlags info
      <*> readUID   base

-- | Given a 'Username' and an exclusive 'Lock'
-- over his mail, returns a 'Sequence' of all the
-- messages in his mailobx.
--
-- As the user must be authenticated at this point,
-- if there is a failure parsing the messages it
-- means his mailbox is corrupt or malformed.
-- Therefore, in those cases, an exception is thrown.
--
-- Throws 'SysErr' on failure.
fetchMailbox :: Lock -> Username -> App (Seq Message)
fetchMailbox _ user = do
  maybeMsgs <- mapM buildMessage <$> maildirFiles user

  case maybeMsgs of
    Nothing   -> UIO.throwIO corruptMailErr
    Just msgs -> pure $ Seq.fromList msgs


-- Message Storage ------------------------------------------------------

-- | Maps a Maildir flag to it's
-- correspondig character representation.
flagToChar :: Flag -> Char
flagToChar = \case
  Seen    -> 'S'
  Replied -> 'R'
  Trashed -> 'T'
  Draft   -> 'D'
  Passed  -> 'P'

-- | Maps the flags of the provided message
-- into it's string representation.
infoMessage :: Message -> String
infoMessage = map flagToChar . Set.toList . msgFlags

-- | Returns the directory where a message
-- should be stored, based on it's flags.
--
-- Read messages are stored in the @cur@
-- directory while the rest are stored on @new@.
dirMessage :: Message -> FilePath
dirMessage msg
  | hasFlag Seen msg = curName
  | otherwise        = newName

-- | Deletes a message by unlinking it's corresponding file.
--
-- Throws 'SysErr' on failure.
deleMessage :: Message -> IO ()
deleMessage = tryUnlink . msgPath

-- | Moves a message to the provided directory by
-- renaming it's corresponding file.
--
-- In the Maildir format, if a message has been
-- read, it is stored in the @cur@ directory and
-- contains the flags suffix @:2<flags>@.
-- Otherwise, it is stored on the @new@ directory
-- and it doesn't have the flags suffix.
--
-- Throws 'SysErr' on failure.
moveMessage :: FilePath -> Message -> IO ()
moveMessage root msg = tryMove src (root </> dir </> dst)
  where
    src = msgPath msg
    dir = dirMessage msg
    dst = (unUID . msgUid) msg <> mailSep <> infoMessage msg

-- | Given a 'Username' and an exclusive 'Lock'
-- over his mail, saves all changes done to the
-- messages by writing them on storage.
--
-- Messages on the first list are deleted while
-- messages on the second one are saved into the
-- appropiate directory.
--
-- Throws 'SysErr' on failure.
updateMailbox :: Lock -> Username -> [Message] -> [Message] -> App ()
updateMailbox _ user trash seen = do
  mailbox <- userMailbox user

  liftIO $ mapM_ (moveMessage mailbox) seen
  liftIO $ mapM_ deleMessage trash
