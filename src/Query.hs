{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Query
  ( Query(..)
  , QueryErr(..)
  , buildQuery
  ) where

import Data.Char (toUpper)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Server (ClientErr(..))
import Types (MsgNo, readMsgNo)

data Query
  = User ByteString
  | Pass ByteString
  | Stat
  | List (Maybe MsgNo)
  | Uidl (Maybe MsgNo) 
  | Retr MsgNo
  | Dele MsgNo
  | Noop
  | Rset
  | Quit
  deriving Show

data QueryErr
  = Client ClientErr
  | Empty
  | Unknown
  | Malformed

instance Show QueryErr where
  show = \case
    Client err -> show err
    Empty      -> "empty query"
    Unknown    -> "unknown command"
    Malformed  -> "malformed command"

parseVoid :: Query -> [ByteString] -> Either QueryErr Query
parseVoid q [] = Right q
parseVoid _ _  = Left Malformed

parseMsgNo :: (MsgNo -> Query) -> [ByteString] -> Either QueryErr Query
parseMsgNo q [arg] = maybe (Left Malformed) (Right . q) (readMsgNo arg)
parseMsgNo _  _    = Left Malformed

parseOptMsgNo :: (Maybe MsgNo -> Query) -> [ByteString] -> Either QueryErr Query
parseOptMsgNo q []    = Right (q Nothing)
parseOptMsgNo q [arg] = parseMsgNo (q . Just) [arg]
parseOptMsgNo _  _    = Left Malformed

parseString :: (ByteString -> Query) -> [ByteString] -> Either QueryErr Query
parseString q [arg] = Right (q arg)
parseString _ _     = Left Malformed

upperFirst :: [ByteString] -> Maybe (ByteString, [ByteString])
upperFirst []     = Nothing
upperFirst (x:xs) = Just (BS.map toUpper x, xs)

tokenize :: ByteString -> Maybe (ByteString, [ByteString])
tokenize = upperFirst . BS.words

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

parserOf :: ByteString -> Maybe ([ByteString] -> Either QueryErr Query)
parserOf = flip lookup parsers

parseQuery :: ByteString -> Either QueryErr Query
parseQuery line = case tokenize line of
  Nothing          -> Left Empty
  Just (cmd, args) ->
    case parserOf cmd of
      Just parser -> parser args
      Nothing     -> Left Unknown
    
buildQuery :: Either ClientErr ByteString -> Either QueryErr Query
buildQuery = either (Left . Client) parseQuery
