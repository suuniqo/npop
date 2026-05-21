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
  , QueryResult
    -- * Builder
  , parseQuery
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

-- | Encodes a query action that can fail
type QueryResult = Either QueryErr Query


-- Parsing --------------------------------------------------------------

-- | Validates a query without arguments.
parseVoid :: Query -> [ByteString] -> Maybe Query
parseVoid q [] = Just q
parseVoid _ _  = Nothing

-- | Validates a query with one message number argument.
parseMsgNo :: (MsgNo -> Query) -> [ByteString] -> Maybe Query
parseMsgNo q [arg] = q <$> readMsgNo arg
parseMsgNo _  _    = Nothing

-- | Validates a query with an optional message number argument.
parseOptMsgNo :: (Maybe MsgNo -> Query) -> [ByteString] -> Maybe Query
parseOptMsgNo q []    = Just (q Nothing)
parseOptMsgNo q [arg] = parseMsgNo (q . Just) [arg]
parseOptMsgNo _  _    = Nothing

-- | Validates a query with one string argument.
parseString :: (ByteString -> Query) -> [ByteString] -> Maybe Query
parseString q [arg] = Just (q arg)
parseString _ _     = Nothing

-- | Normalizes a query command by making the
-- first token uppercase, as POP3 is case insensitive.
upperFirst :: [ByteString] -> Maybe (ByteString, [ByteString])
upperFirst []     = Nothing
upperFirst (x:xs) = Just (BS.map toUpper x, xs)

-- | Tokenizes a query by splitting the whitespace separated
-- words and making the first one uppercase.
tokenize :: ByteString -> Maybe (ByteString, [ByteString])
tokenize = upperFirst . BS.words

-- | Encodes a Query parser
type QueryParser = [ByteString] -> Maybe Query

-- | Maps each query command to a parser which validates it's arguments.
parsers :: [(ByteString, QueryParser)]
parsers = 
  [ ("USER", parseString   User)
  , ("PASS", parseString   Pass)
  , ("LIST", parseOptMsgNo List)
  , ("UIDL", parseOptMsgNo Uidl)
  , ("RETR", parseMsgNo    Retr)
  , ("DELE", parseMsgNo    Dele)
  , ("STAT", parseVoid     Stat)
  , ("NOOP", parseVoid     Noop)
  , ("RSET", parseVoid     Rset)
  , ("QUIT", parseVoid     Quit)
  ]

-- | Given a POP3 command, returns a parser which validats it's arguments.
-- Returns 'Nothing' if the command is unknown.
parserOf :: ByteString -> Maybe QueryParser
parserOf = flip lookup parsers

-- | Given a raw POP3 query, parses it into a validated 'Query'.
-- Returns 'Left' with 'QueryErr' if it can't be parsed or validated.
parseQuery' :: ByteString -> QueryResult
parseQuery' line = case tokenize line of
  Nothing          -> Left Empty
  Just (cmd, args) ->
    case parserOf cmd of
      Just parser -> maybe (Left Malformed) Right (parser args)
      Nothing     -> Left Unknown


-- Builder --------------------------------------------------------------

-- | Given a raw POP3 query, parses it into a validated 'Query'.
-- Returns 'Left' with 'QueryErr' if it can't be parsed or validated.
-- If there was an error receiving the raw query, maps the 'ClientErr' to a 'QueryErr'.
parseQuery :: Either ClientErr ByteString -> QueryResult
parseQuery = either (Left . Client) parseQuery'
