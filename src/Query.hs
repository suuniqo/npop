{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module      : Query
Description : POP3 query parsing
Portability : POSIX

Parsing and validation of POP3 queries into the 'Query' type.
-}
module Query
  ( -- * Types
    Query(..)
  , QueryErr(..)
    -- * Builder
  , buildQuery
  ) where


-- Imports --------------------------------------------------------------

import Data.Char (toUpper)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)

import Server (ClientErr (..))
import Types (MsgNo, readMsgNo)


-- Types ----------------------------------------------------------------

-- | Encodes a validated POP3 query,
-- along with it's arguments
data Query
  = User ByteString         -- ^ USER command, with the given username
  | Pass ByteString         -- ^ PASS command, with the given password
  | Stat                    -- ^ STAT command
  | List (Maybe MsgNo)      -- ^ LIST command, with an optional message number
  | Uidl (Maybe MsgNo)      -- ^ UIDL command, with an optional message number
  | Retr MsgNo              -- ^ RETR command, with the given message number
  | Dele MsgNo              -- ^ DELE command, with the given message number
  | Noop                    -- ^ NOOP command
  | Rset                    -- ^ RSET command
  | Quit                    -- ^ QUIT command
  deriving Show

-- | Encodes a failure when parsing
-- or validating a POP3 query.
data QueryErr
  = Client ClientErr        -- ^ Client error when receiving the query
  | Empty                   -- ^ Empty query (contains only CRLF)
  | Unknown                 -- ^ Unknown query command
  | Malformed               -- ^ Malformed query (invalid argument number or format)

instance Show QueryErr where
  show = \case
    Client err -> show err
    Empty      -> "empty query"
    Unknown    -> "unknown command"
    Malformed  -> "malformed command"


-- Parsing --------------------------------------------------------------

-- | Validates a query without arguments.
parseVoid :: Query -> [ByteString] -> Either QueryErr Query
parseVoid q [] = Right q
parseVoid _ _  = Left Malformed

-- | Validates a query with one message number argument.
parseMsgNo :: (MsgNo -> Query) -> [ByteString] -> Either QueryErr Query
parseMsgNo q [arg] = maybe (Left Malformed) (Right . q) (readMsgNo arg)
parseMsgNo _  _    = Left Malformed

-- | Validates a query with an optional message number argument.
parseOptMsgNo :: (Maybe MsgNo -> Query) -> [ByteString] -> Either QueryErr Query
parseOptMsgNo q []    = Right (q Nothing)
parseOptMsgNo q [arg] = parseMsgNo (q . Just) [arg]
parseOptMsgNo _  _    = Left Malformed

-- | Validates a query with one string argument.
parseString :: (ByteString -> Query) -> [ByteString] -> Either QueryErr Query
parseString q [arg] = Right (q arg)
parseString _ _     = Left Malformed

-- | Normalizes a query command by making the
-- first token uppercase, as POP3 is case insensitive.
upperFirst :: [ByteString] -> Maybe (ByteString, [ByteString])
upperFirst []     = Nothing
upperFirst (x:xs) = Just (BS.map toUpper x, xs)

-- | Tokenizes a query by splitting the whitespace separated
-- words and making the first one uppercase.
tokenize :: ByteString -> Maybe (ByteString, [ByteString])
tokenize = upperFirst . BS.words

-- | Maps each query command to a parser which validates it's arguments.
parsers :: [(ByteString, [ByteString] -> Either QueryErr Query)]
parsers = 
  [ ("USER", parseString User)
  , ("PASS", parseString Pass)
  , ("LIST", parseOptMsgNo List)
  , ("UIDL", parseOptMsgNo Uidl)
  , ("RETR", parseMsgNo Retr)
  , ("DELE", parseMsgNo Dele)
  , ("STAT", parseVoid Stat)
  , ("NOOP", parseVoid Noop)
  , ("RSET", parseVoid Rset)
  , ("QUIT", parseVoid Quit)
  ]

-- | Given a POP3 command, returns a parser which validats it's arguments.
-- Returns 'Nothing' if the command is unknown.
parserOf :: ByteString -> Maybe ([ByteString] -> Either QueryErr Query)
parserOf = flip lookup parsers

-- | Given a raw POP3 query, parses it into a validated 'Query'.
-- Returns 'Left' with 'QueryErr' if it can't be parsed or validated.
parseQuery :: ByteString -> Either QueryErr Query
parseQuery line = case tokenize line of
  Nothing          -> Left Empty
  Just (cmd, args) ->
    case parserOf cmd of
      Just parser -> parser args
      Nothing     -> Left Unknown


-- Builder --------------------------------------------------------------

-- | Given a raw POP3 query, parses it into a validated 'Query'.
-- Returns 'Left' with 'QueryErr' if it can't be parsed or validated.
-- If there was an error receiving the raw query, maps the 'ClientErr' to a 'QueryErr'.
buildQuery :: Either ClientErr ByteString -> Either QueryErr Query
buildQuery = either (Left . Client) parseQuery
