{-# LANGUAGE LambdaCase #-}

{-|
Module      : Main
Description : Script to automate Maildir operations
Portability : POSIX

Small script bundling the operations of adding and
deleting users, as well as locally sending mail to them.
-}
module Main where


-- Imports --------------------------------------------------------------

import Control.Monad (when, unless)
import qualified Data.ByteString.Char8 as BS
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))
import System.IO (hSetEcho, stdin, hFlush, stdout, stderr, hPutStr, isEOF)
import System.Posix (getProcessID, epochTime, getSystemID, SystemID (nodeName))

import Crypto.BCrypt

import App (buildEnv, AppEnv (shadow, config), BuildErr (ConfigErr, ShadowErr))
import Config (Config(storage), StorageConfig (mailRoot, passName))
import Constant (curName, newName, tmpName)
import Shadow (userExists)


-- Maildir Layout -------------------------------------------------------

-- | Extracts the shadow file path,
-- based on the app configuration.
shadowPath :: AppEnv -> FilePath
shadowPath env =
  let conf = (storage . config) env
  in  mailRoot conf </> passName conf

-- | Given a username, extracts it's root mail directory,
-- based on the app configuration.
userDir :: AppEnv -> String -> FilePath
userDir env user =
  let conf = (storage . config) env
  in  mailRoot conf </> user

-- | Given a username, extracts it's 'cur', 'new' and 'tmp'
-- mail directories, based on the app configuration.
mailDirs :: AppEnv -> String -> [FilePath]
mailDirs env user =
  let root = userDir env user
  in [root </> newName, root </> curName, root </> tmpName]


-- Putting a Message ----------------------------------------------------

-- | Generates the file name of a new mail following
-- the Maildir format of: @<timestamp>.<pid>.<hostname>@.
mailUID :: IO String
mailUID = do
  time <- epochTime
  pid <- getProcessID
  host <- nodeName <$> getSystemID
  pure $ intercalate "." [show time, show pid, host]

-- | Reads input from 'stdin' until the user enters EOF, and returns the contents.
readUntilEOF :: IO String
readUntilEOF = do
  eof <- isEOF
  if eof
    then pure ""
    else do
      line <- getLine
      rest <- readUntilEOF
      pure (line <> "\n" <> rest)

-- | Puts a new message into the 'new' mail directory of the provided user.
-- The message file name is created following the Maildir format.
putMail :: String -> AppEnv -> IO ()
putMail user env =
  do
    unless (userExists (shadow env) user) $ die "user doesn't exist"
    path <- (newMailDir </>) <$> mailUID
    hPutStr stderr "Message:\n"
    readUntilEOF >>= writeFile path
  where
    newMailDir = head $ mailDirs env user


-- Deleting a User ------------------------------------------------------

-- | Deletes a user by removing it's shadow file entry and mail directories.
delUser :: String -> AppEnv -> IO ()
delUser user env = do
  unless (userExists (shadow env) user) $ die "user doesn't exist"

  let deleted = Map.delete user (shadow env)
  let contents = unlines $ map (\(u, h) -> u <> ":" <> h) (Map.toList deleted)

  writeFile (shadowPath env) contents
  removeDirectoryRecursive (userDir env user)


-- Adding a User --------------------------------------------------------

-- | Generates the bcrypt hash of a plaintext password.
genHash :: String -> IO String
genHash pass =
  hashPasswordUsingPolicy slowerBcryptHashingPolicy (BS.pack pass)
    >>= maybe (die "hashing failed") (pure . BS.unpack)

-- | Prompts the user to enter a password,
-- disabling echo so that it isn't leaked.
promptPass :: IO String
promptPass = do
  hPutStr stderr "Password: "
  hFlush stdout
  hSetEcho stdin False
  pass <- getLine
  hSetEcho stdin True
  hPutStr stderr "\n"
  pure pass

-- | Creates a new virtual user by adding it's username and password
-- hash to the shadow file and creating it's mail directories.
addUser :: String -> AppEnv -> IO ()
addUser user env = do
  when (userExists (shadow env) user) $ die "user already exists"

  pass <- promptPass
  hash <- genHash pass

  let entry = user <> ":" <> hash <> "\n"
  appendFile (shadowPath env) entry

  mapM_ (createDirectoryIfMissing True) (mailDirs env user)


-- Startup --------------------------------------------------------------

-- | Tries to load the application environment. On failure the program dies.
getEnv :: IO AppEnv
getEnv = buildEnv >>= \case
  Left (ConfigErr err) -> die $ "npop-adduser: failed to build config: " <> show err
  Left (ShadowErr err) -> die $ "npop-adduser: failed to build shadow: " <> show err
  Right env            -> pure env

-- | Selects the appropiate mode based on the args
-- and injects the application environment into it.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["add", user] -> getEnv >>= addUser user
    ["del", user] -> getEnv >>= delUser user
    ["put", user] -> getEnv >>= putMail user
    _             -> die "usage: npop-tools add|del|put <username>"
