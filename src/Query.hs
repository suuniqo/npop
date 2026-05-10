{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Query
  ( Query(..)
  , QueryErr(..)
  , MsgNo
  , toIdx
  , msgEnum
  , buildQuery
  ) where

import Data.Char (toUpper)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Server (ClientErr(..))

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

newtype MsgNo = MsgNo Int
  deriving (Show, Eq, Ord)

toIdx :: MsgNo -> Int
toIdx (MsgNo num) = num - 1

msgEnum :: [MsgNo]
msgEnum = MsgNo <$> [1..]

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

maxWord :: Integer
maxWord = toInteger (maxBound :: Int)

readNo :: BS.ByteString -> Maybe MsgNo
readNo bs = do
    (n, rest) <- BS.readInteger bs

    if BS.null rest
       && n > 0
       && n <= maxWord
    then Just (MsgNo $ fromInteger n)
    else Nothing

parseVoid :: Query -> [ByteString] -> Either QueryErr Query
parseVoid q [] = Right q
parseVoid _ _  = Left Malformed

parseNo :: (MsgNo -> Query) -> [ByteString] -> Either QueryErr Query
parseNo q [arg] = maybe (Left Malformed) (Right . q) (readNo arg)
parseNo _  _    = Left Malformed

parseMaybeNo :: (Maybe MsgNo -> Query) -> [ByteString] -> Either QueryErr Query
parseMaybeNo q []    = Right (q Nothing)
parseMaybeNo q [arg] = parseNo (q . Just) [arg]
parseMaybeNo _  _    = Left Malformed

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
  , ("LIST", parseMaybeNo List)
  , ("UIDL", parseMaybeNo Uidl)
  , ("RETR", parseNo Retr)
  , ("DELE", parseNo Dele)
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
