module Types
  ( Username (..)
  , MsgNo, toIdx, msgEnum
  , UID (..)
  ) where

-- Username

newtype Username = Username String

-- MsgNo

newtype MsgNo = MsgNo Int
  deriving (Show, Eq, Ord)

toIdx :: MsgNo -> Int
toIdx (MsgNo num) = num - 1

msgEnum :: [MsgNo]
msgEnum = MsgNo <$> [1..]

-- UID

newtype UID = UID { unUID :: String }
