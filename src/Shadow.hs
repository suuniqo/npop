{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Shadow
Description : Password file parsing and handling.
Portability : POSIX

Manges the shadow file, which contains user password hashes in the format <username>:<hash>,
and is used to authenticate and validate remote clients. Hashes are generated using BCrypt.
-}
module Shadow
  ( -- * Types
    Shadow
    -- * Loader
  , loadShadow
    -- * Operations
  , userExists
  , auth
  ) where


-- Imports --------------------------------------------------------------

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)

import Crypto.BCrypt (validatePassword)

import Types (Username (unUser))


-- Types ----------------------------------------------------------------

-- | Holds the shadow file data, which
-- maps users to their password hashes.
type Shadow = Map String String
  

-- Loading --------------------------------------------------------------

-- | Builds 'Shadow from the indicated file.
--
-- Throws 'IOException' on failure.
loadShadow :: FilePath -> IO Shadow
loadShadow path =
  do
    contents <- readFile path
    pure $ Map.fromList (parseLine <$> lines contents)
  where
    parseLine line =
      let (user, rest) = break (== ':') line
      in  (user, drop 1 rest)


-- Operations -----------------------------------------------------------

-- | Given 'Shadow' and a username, returns
-- whether the user is present in the file or,
-- in other words, whether it is registered.
userExists :: Shadow -> String -> Bool
userExists shdw user = Map.member user shdw

-- | Dummy BCrypt hash of the word "dummyHash".
dummyHash :: ByteString
dummyHash = "$2y$14$vsdzkSvDEghs5zrHgxvlJ.5kyx3v/p5ZvpNAetf5RGtPu2KyhtREW"

-- | Authenticates a user. To succeed, the user must be
-- present on 'Shadow' and it's hash must be equal
-- to the hash of the provided password.
--
-- Returning 'False' immediately if the user doesn't
-- exist could help attackers to figure out which
-- usernames are valid.
--
-- To avoid this kind of timing attack, even if a user
-- doesn't exist, it's password is validated with a dummy hash.
auth :: Shadow -> Username -> ByteString -> Bool
auth shadow user pass =
  let (hash, exists) = case Map.lookup (unUser user) shadow of
        Nothing -> (validatePassword dummyHash pass, False)     -- fake validation to avoid timing attacks
        Just hs -> (validatePassword (BS.pack hs) pass, True)   -- real validation against the shadow hash
  in hash && exists
