{-|
Module      : App
Description : Application monad and environment builder.
Portability : POSIX

Defines the 'App' monad that threads the application environment
through the entire server, and 'buildEnv' which loads configuration
and the shadow file at startup.
-}
module App
  ( -- * Types
    AppEnv (..)
    -- * Builder
  , BuildErr (..)
  , buildEnv
    -- * Monad
  , App
  , runApp
  ) where


-- Imports --------------------------------------------------------------

import Control.Exception (SomeException, catch)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import System.Directory (getXdgDirectory, XdgDirectory (XdgConfig))
import System.FilePath ((</>))

import Constant (progName, configName)
import Config (Config (storage), loadConfig, StorageConfig (mailRoot, passName))
import Shadow (Shadow, loadShadow)


-- Types ----------------------------------------------------------------

-- | Shared read-only environment passed around via the 'App' monad.
-- It is loaded once at startup and never mutated.
data AppEnv = AppEnv
  { config :: Config    -- ^ Application configuration
  , shadow :: Shadow    -- ^ Application shadow file
  } deriving Show


-- Builder --------------------------------------------------------------

-- | Builds the config file from the XDG config directory.
--
-- Looks for @$XDG_CONFIG_HOME\/npop\/config.toml@, falling back to
-- @~\/.config\/npop\/config.toml@ if the environment variable is not set.
--
-- Throws 'IOException' on failure.
buildConfig :: IO Config
buildConfig = do
  root <- getXdgDirectory XdgConfig progName
  loadConfig $ root </> configName

-- | Loads the shadow file from the mail root specified in 'Config'.
--
-- Throws 'IOException' on failure
buildShadow :: Config -> IO Shadow
buildShadow conf =
  let root = mailRoot $ storage conf
      name = passName $ storage conf
  in  loadShadow (root </> name)
  
-- | Errors that can occur while loading and
-- building the application environment.
data BuildErr
  = ConfigErr SomeException     -- ^ Failed to load or parse the config file
  | ShadowErr SomeException     -- ^ Failed to load or parse the shadow file

-- | Builds the application environment from the config and shadow files.
-- Returns 'Left' with a 'BuildErr' if either step fails.
buildEnv :: IO (Either BuildErr AppEnv)
buildEnv = runExceptT $ do
  conf <- ExceptT $ catch (Right <$> buildConfig)      (pure . Left . ConfigErr)
  shdw <- ExceptT $ catch (Right <$> buildShadow conf) (pure . Left . ShadowErr)
  pure $ AppEnv conf shdw


-- Monad ----------------------------------------------------------------

-- | The application monad, built by adding the
-- read-only AppEnv environment to the 'IO' monad.
type App = ReaderT AppEnv IO

-- | Unwraps an 'App' action into 'IO' given an 'AppEnv'.
runApp :: App a -> AppEnv -> IO a
runApp = runReaderT
