{- |
Module      : Constant
Description : Application and protocol constants
Portability : POSIX

Constant values used throughout the application.
-}
module Constant
  ( -- * General
    progName
  , configName
    -- * Protocol
  , queryMaxLen
  , uidMaxLen
    -- * Maildir
  , curName
  , newName
  , mailSep
  ) where


-- General --------------------------------------------------------------

-- | Name of the application
progName :: FilePath
progName = "npop"

-- | Name of the config file
configName :: FilePath
configName = "config.toml"


-- Protocol -------------------------------------------------------------

-- | Maximum length of a POP3 keyword
queryKwdLen :: Int
queryKwdLen = 4

-- | Maximum length of a POP3 argument
queryArgLen :: Int
queryArgLen = 40

-- | Maximum length of a POP3 query,
-- including the terminating CRLF.
queryMaxLen :: Int
queryMaxLen = queryKwdLen + 1 + queryArgLen + 2

-- | Maximum length of a POP3 message UID.
uidMaxLen :: Int
uidMaxLen = 70


-- Maildir --------------------------------------------------------------

-- | Name of the seen mail directory.
curName :: FilePath
curName = "cur"

-- | Name of the unseen mail directory.
newName :: FilePath
newName = "new"

-- | UID and flag separator of mail.
mailSep :: FilePath
mailSep = ":2"
