{- |
Module      : Types
Description : Shared domain types
Portability : POSIX

Shared types used throughout the application.
-}
module Types
  ( -- * Username
    Username (..)
  , userValidate
    -- * MsgNo
  , MsgNo
  , toIdx
  , msgEnum
  , readMsgNo
    -- * UID
  , UID (..)
  , readUID
    -- * Message
  , Flag (..)
  , Message (..)
  , hasFlag
  , addFlag
  ) where


-- Imports --------------------------------------------------------------

import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import Data.Char (isAlphaNum)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Time.Clock.POSIX (POSIXTime)

import Constant (uidMaxLen)


-- Username -------------------------------------------------------------

-- | Validated alphanumeric username.
newtype Username = Username { unUser :: String }

instance Show Username where
  show = unUser

-- | Constructs a 'Username' by validating a 'ByteString'.
-- Returns 'Nothing' if the input isn't alphanumeric.
--
-- The validation is done to avoid query injections.
userValidate :: ByteString -> Maybe Username
userValidate user
  | BS.all isAlphaNum user = Just $ Username (BS.unpack user)
  | otherwise              = Nothing


-- MsgNo ----------------------------------------------------------------

-- | Validated postive message number.
newtype MsgNo = MsgNo Int
  deriving (Ord, Eq)

instance Show MsgNo where
  show (MsgNo num) = show num

-- | As POP3 message indices must start at 1, the number is
-- substracted one to convert it to an index starting at 0.
toIdx :: MsgNo -> Int
toIdx (MsgNo num) = num - 1

-- | Infinite list containing all possible
-- values of 'MsgNo' in decreasing order.
--
-- This function is useful to enumerate
-- lists of messages with their real index:
--
-- >>> zip [msg1, msg2, msg3] msgEnum
-- [(msg1, MsgNo 1), (msg2, MsgNo 2), (msg3, MsgNo 3)]
msgEnum :: [MsgNo]
msgEnum = MsgNo <$> [1..]

-- | Maximum 'Int' as 'Integer'
maxInt :: Integer
maxInt = toInteger (maxBound :: Int)

-- | Parses and validates a 'MsgNo' by first checking if
-- it's a valid 'Integer' and then checking if it's positive.
--
-- Examples:
--
-- >>> readMsgNo "-1"
-- Nothing
--
-- >>> readMsgNo "0"
-- Nothing
--
-- >>> readMsgNo "1"
-- Just (MsgNo 1)
--
readMsgNo :: BS.ByteString -> Maybe MsgNo
readMsgNo bs = do
    (n, rest) <- BS.readInteger bs

    if BS.null rest
       && n > 0
       && n <= maxInt
    then Just (MsgNo $ fromInteger n)
    else Nothing


-- UID ------------------------------------------------------------------

-- | Validated message uniquer identifier.
newtype UID = UID { unUID :: String }

instance Show UID where
  show = unUID

-- | Constructs a POP3 'UID' by validating a 'String'.
-- Returns 'Nothing' if lenfth is greater than
-- the maximum uid length or if any characters
-- are outside the defined valid range.
readUID :: String -> Maybe UID
readUID string
  | length string > uidMaxLen = Nothing
  | not $ all inRange string  = Nothing
  | otherwise                 = Just (UID string)
  where
    inRange c = c >= '!' && c <= '~'


-- Message --------------------------------------------------------------

-- | Encodes the different flags which
-- the Maildir format defines for mail.
data Flag
  = Draft       -- ^ Is a draft
  | Passed      -- ^ Has been forwared
  | Replied     -- ^ Has been replied to
  | Seen        -- ^ Has been read
  | Trashed     -- ^ Has been trashed
  deriving (Show, Eq, Ord)

-- | Encodes a mail message.
data Message = Message
  { msgPath  :: FilePath    -- ^ Path of the message
  , msgSize  :: Integer     -- ^ Size of the message
  , msgTime  :: POSIXTime   -- ^ Delivery timestamp
  , msgFlags :: Set Flag    -- ^ Flags of the message
  , msgUid   :: UID         -- ^ UID of the message
  } deriving Show

-- | Checks if the message contains the provided flag.
hasFlag :: Flag -> Message -> Bool
hasFlag flag = Set.member flag . msgFlags

-- | Given a message, it inserts the provided flag into it's flag set.
addFlag :: Flag -> Message -> Message
addFlag flag msg = 
  let newFlags = Set.insert flag (msgFlags msg)
  in  msg { msgFlags = newFlags }
