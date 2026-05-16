{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Config
Description : Customizable application data.
Portability : POSIX

Loads customizable data from an optional TOML config file,
and builds it into the 'Config' type.
-}
module Config
  ( -- * Types
    Config (..)
  , StorageConfig (..)
  , NetworkConfig (..)
    -- * Loader
  , loadConfig
  ) where


-- Imports --------------------------------------------------------------

import Control.Exception (try, IOException)

import qualified Toml
import Toml (TomlCodec, (.=))

import Log (Severity(Warn), emit)


-- Types ----------------------------------------------------------------

-- | Storage application settings.
data StorageConfig = StorageConfig
  { mailRoot :: FilePath        -- ^ Root directory for user mailboxes
  , passName :: String          -- ^ Password file name
  , lockName :: String          -- ^ Mailbox lock file name
  } deriving Show

-- | Network and I/O tuning settings.
data NetworkConfig = NetworkConfig
  { port          :: String     -- ^ Service name or port number
  , idleTimeout   :: Int        -- ^ autologout timeout for idle clients
  , listenBacklog :: Int        -- ^ TCP listen backlog queue size
  , backoffMin    :: Int        -- ^ Minimum retry backoff in microseconds
  , backoffMax    :: Int        -- ^ Maximum retry backoff in microseconds
  , readChunk     :: Int        -- ^ Bytes per read syscall
  } deriving Show

-- | Global configuration data.
data Config = Config
  { storage :: StorageConfig    -- ^ Storage configuration
  , network :: NetworkConfig    -- ^ Network configuration
  } deriving Show


-- Codecs ---------------------------------------------------------------

-- | Storage configuration TOML decoder.
storageCodec :: TomlCodec StorageConfig
storageCodec = StorageConfig
  <$> Toml.string "mail_root" .= mailRoot
  <*> Toml.string "pass_name" .= passName
  <*> Toml.string "lock_name" .= lockName

-- | Network configuration TOML decoder.
networkCodec :: TomlCodec NetworkConfig
networkCodec = NetworkConfig
  <$> Toml.string "port"        .= port
  <*> Toml.int "idle_timeout"   .= idleTimeout
  <*> Toml.int "listen_backlog" .= listenBacklog
  <*> Toml.int "backoff_min"    .= backoffMin
  <*> Toml.int "backoff_max"    .= backoffMax
  <*> Toml.int "read_chunk"     .= readChunk

-- | Global configuration TOML decoder.
--
-- Storage configuration is contained in the [storage] table,
-- while network configuration is in the [network] one.
configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.table storageCodec "storage" .= storage
  <*> Toml.table networkCodec "network" .= network


-- Loading --------------------------------------------------------------

-- | Default configuration values, used as fallback
-- in case of a missing configuration file or a malformed one.
defaultConfig :: Config
defaultConfig = Config
  { storage = StorageConfig
      { passName = "shadow"
      , lockName = ".lock"
      , mailRoot = "/var/npop"
      }
  , network = NetworkConfig
      { port          = "pop3"
      , idleTimeout   = 600000000
      , listenBacklog = 16
      , backoffMin    = 62500
      , backoffMax    = 16000000
      , readChunk     = 4096
      }
  }

-- | Builds 'Config' from the indicated TOML file.
--
-- In case of an error or a missing file a warning is
-- emitted and the default configuration is used instead.
loadConfig :: FilePath -> IO Config
loadConfig path = do
  result <- try $ Toml.decodeFileEither configCodec path

  case result of
    Left (_ :: IOException) -> do
      emit Warn "no config file, using defaults"
      pure defaultConfig
    Right (Left err) -> do
      emit Warn $ "config err, using defaults: " <> show err
      pure defaultConfig
    Right (Right config) -> pure config
