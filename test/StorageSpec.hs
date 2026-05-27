{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : StorageSpec
Description : Hspec unit tests for the Storage module
Portability : POSIX

Tests storage operations against a real temporary Maildir
tree newly created for each test and removed afterwards.
-}
module StorageSpec (spec) where


-- Imports --------------------------------------------------------------

import Control.Exception (SomeException, try, throwIO)
import Data.ByteString.Char8 (pack)
import qualified Data.Foldable as Seq
import Data.List (sort)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , listDirectory
  , removeDirectoryRecursive
  )
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import Test.Hspec

import App (App, AppEnv (..), runApp)
import Config (Config (..), StorageConfig (..))
import Constant (curName, mailSep, newName)
import Storage
  ( StorageErr (..)
  , Lock
  , fetchMailbox
  , updateMailbox
  , withLock
  )
import Types
  ( Flag (..)
  , Message (..)
  , Username
  , addFlag
  , hasFlag
  , userValidate
  )


-- Config ---------------------------------------------------------------

-- | Runs a storage 'App' action against the given temporary root.
run :: FilePath -> App a -> IO a
run tmp app = runApp app (testEnv tmp)

-- | Username string representation used across all tests.
testUserRaw :: String
testUserRaw = "testuser"

-- | Username used across all tests.
testUser :: Username
testUser = case userValidate (pack testUserRaw) of
  Just u  -> u
  Nothing -> error "StorageSpec: invalid test username"

-- | Minimal 'AppEnv' pointing storage at the given temporary root.
-- The 'shadow' field is 'undefined' because no storage function
-- checks credentials.
testEnv :: FilePath -> AppEnv
testEnv tmp = AppEnv
  { config = Config
      { storage = StorageConfig { mailRoot = tmp, passName = "shadow", lockName = ".lock" }
      , network = undefined   -- not used by storage tests
      }
  , shadow = undefined        -- not used by storage tests
  }


-- Helpers --------------------------------------------------------------

-- | Creates a new Maildir tree for 'testUser', passes the root to
-- the action, and removes it afterwards, including on exceptions.
withMaildir :: (FilePath -> IO ()) -> IO ()
withMaildir action = do
  tmp <- createTempDirectory "/tmp" "npop-test-"
  let mbox = tmp </> testUserRaw
  createDirectoryIfMissing True (mbox </> curName)
  createDirectoryIfMissing True (mbox </> newName)
  result <- try (action tmp) :: IO (Either SomeException ())
  removeDirectoryRecursive tmp
  either (throwIO . userError . show) pure result

-- | Writes a file into the @new@ directory of 'testUser's mailbox.
sendNew :: FilePath -> FilePath -> String -> IO ()
sendNew tmp name = writeFile (tmp </> testUserRaw </> newName </> name)

-- | Writes a file into the @cur@ directory of 'testUser's mailbox.
sendCur :: FilePath -> FilePath -> String -> IO ()
sendCur tmp name = writeFile (tmp </> testUserRaw </> curName </> name)

-- | Acquires a lock for 'testUser' and runs an action, failing the
-- test immediately if the lock cannot be acquired.
withTestLock :: FilePath -> (Lock -> App a) -> IO a
withTestLock tmp action =
  run tmp (withLock testUser action) >>= \case
    Left  e -> expectationFailure (show e) >> undefined
    Right a -> pure a

-- | Fetches all messages for 'testUser' inside a lock.
fetchAll :: FilePath -> IO [Message]
fetchAll tmp = Seq.toList <$> withTestLock tmp (`fetchMailbox` testUser)

-- | Counts files across @cur@ and @new@.
countFiles :: FilePath -> IO Int
countFiles tmp = do
  cs <- listDirectory (tmp </> testUserRaw </> curName)
  ns <- listDirectory (tmp </> testUserRaw </> newName)
  pure (length cs + length ns)

-- | Appends the Maildir flag separator and the given
-- flags to a base name, producing a valid cur filename.
withFlags :: FilePath -> String -> FilePath
withFlags base flags = base <> mailSep <> flags

-- | Lists filenames in @cur@.
curFiles :: FilePath -> IO [FilePath]
curFiles tmp = sort <$> listDirectory (tmp </> testUserRaw </> curName)


-- withLock -------------------------------------------------------------

withLockSpec :: Spec
withLockSpec = around withMaildir $ describe "withLock" $ do

  it "returns Right when mailbox is unlocked" $ \tmp -> do
    result <- run tmp (withLock testUser $ \lock -> pure ())
    result `shouldBe` Right ()

  it "returns Left UserLocked when already locked" $ \tmp -> do
    result <- run tmp $ withLock testUser $ \outer ->
      withLock testUser $ \inner -> pure ()
    result `shouldBe` Right (Left UserLocked)

  it "releases the lock after normal completion" $ \tmp -> do
    _ <- run tmp (withLock testUser $ \lock -> pure ())
    result <- run tmp (withLock testUser $ \lock -> pure ())
    result `shouldBe` Right ()

  it "releases the lock even if the action throws" $ \tmp -> do
    _ :: Either SomeException a <- try $
      run tmp (withLock testUser $ \lock -> fail "error")
    result <- run tmp (withLock testUser $ \lock -> pure ())
    result `shouldBe` Right ()

  it "removes the lock file after release" $ \tmp -> do
    _ <- run tmp (withLock testUser $ \_lock -> pure ())
    exists <- doesFileExist (tmp </> testUserRaw </> ".lock")
    exists `shouldBe` False


-- fetchMailbox ---------------------------------------------------------

fetchMailboxSpec :: Spec
fetchMailboxSpec = around withMaildir $ describe "fetchMailbox" $ do

  it "returns empty sequence for an empty mailbox" $ \tmp -> do
    msgs <- fetchAll tmp
    msgs `shouldBe` []

  it "loads a message from new" $ \tmp -> do
    sendNew tmp "1000000001.1.host" "content"
    msgs <- fetchAll tmp
    length msgs `shouldBe` 1

  it "loads a message from cur" $ \tmp -> do
    sendCur tmp (withFlags "1000000002.1.host" "S") "content"
    msgs <- fetchAll tmp
    length msgs `shouldBe` 1

  it "loads messages from both cur and new" $ \tmp -> do
    sendNew tmp "1000000003.1.host"    "msg1"
    sendCur tmp (withFlags "1000000004.1.host" "S") "msg2"
    msgs <- fetchAll tmp
    length msgs `shouldBe` 2

  it "records the correct file size" $ \tmp -> do
    let content = "From: a@b.com\r\n\r\nBody"
    sendNew tmp "1000000005.1.host" content
    [msg] <- fetchAll tmp
    msgSize msg `shouldBe` fromIntegral (length content)

  it "records the full file path" $ \tmp -> do
    let name = "1000000006.1.host"
    sendNew tmp name "content"
    [msg] <- fetchAll tmp
    msgPath msg `shouldBe` (tmp </> testUserRaw </> newName </> name)

  it "parses the Seen flag from the filename suffix" $ \tmp -> do
    sendCur tmp (withFlags "1000000007.1.host" "S") "content"
    [msg] <- fetchAll tmp
    hasFlag Seen msg `shouldBe` True

  it "parses no flags for a message in new" $ \tmp -> do
    sendNew tmp "1000000008.1.host" "content"
    [msg] <- fetchAll tmp
    msgFlags msg `shouldBe` Set.empty

  it "parses multiple flags from the suffix" $ \tmp -> do
    sendCur tmp (withFlags "1000000009.1.host" "SR") "content"
    [msg] <- fetchAll tmp
    hasFlag Seen    msg `shouldBe` True
    hasFlag Replied msg `shouldBe` True


-- updateMailbox --------------------------------------------------------

updateMailboxSpec :: Spec
updateMailboxSpec = around withMaildir $ describe "updateMailbox" $ do

  it "does nothing when both lists are empty" $ \tmp -> do
    sendNew tmp "1000000020.1.host" "msg"
    before <- countFiles tmp
    withTestLock tmp $ \lock -> updateMailbox lock testUser [] []
    after <- countFiles tmp
    after `shouldBe` before

  it "deletes a message in the trash list" $ \tmp -> do
    sendNew tmp "1000000021.1.host" "msg"
    msgs <- fetchAll tmp
    withTestLock tmp $ \lock -> updateMailbox lock testUser msgs []
    countFiles tmp >>= (`shouldBe` 0)

  it "deletes all messages when all are trashed" $ \tmp -> do
    sendNew tmp "1000000022.1.host" "msg1"
    sendNew tmp "1000000023.1.host" "msg2"
    msgs <- fetchAll tmp
    withTestLock tmp $ \lock -> updateMailbox lock testUser msgs []
    countFiles tmp >>= (`shouldBe` 0)

  it "moves a Seen message from new to cur" $ \tmp -> do
    sendNew tmp "1000000024.1.host" "msg"
    msgs <- fetchAll tmp
    withTestLock tmp $ \lock ->
      updateMailbox lock testUser [] (addFlag Seen <$> msgs)
    curFiles tmp >>= (`shouldSatisfy` ((== 1) . length))
    listDirectory (tmp </> testUserRaw </> newName) >>= (`shouldBe` [])

  it "appends the Seen flag character to the filename on move" $ \tmp -> do
    sendNew tmp "1000000025.1.host" "msg"
    msgs <- fetchAll tmp
    withTestLock tmp $ \lock ->
      updateMailbox lock testUser [] (addFlag Seen <$> msgs)
    [name] <- curFiles tmp
    name `shouldContain` "S"

  it "removes the original new file after the Seen move" $ \tmp -> do
    let name = "1000000026.1.host"
    sendNew tmp name "msg"
    msgs <- fetchAll tmp
    withTestLock tmp $ \lock ->
      updateMailbox lock testUser [] (addFlag Seen <$> msgs)
    exists <- doesFileExist (tmp </> testUserRaw </> newName </> name)
    exists `shouldBe` False

  it "deletes some and moves others in the same call" $ \tmp -> do
    sendNew tmp "1000000027.1.host" "to delete"
    sendNew tmp "1000000028.1.host" "to keep"
    msgs <- fetchAll tmp
    let (trash, keep) = splitAt 1 (sort msgs)
    withTestLock tmp $ \lock ->
      updateMailbox lock testUser trash (addFlag Seen <$> keep)
    countFiles tmp >>= (`shouldBe` 1)
    curFiles tmp  >>= (`shouldSatisfy` ((== 1) . length))


-- Storage Spec ---------------------------------------------------------

spec :: Spec
spec = describe "Storage" $ do
  withLockSpec
  fetchMailboxSpec
  updateMailboxSpec
