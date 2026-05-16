{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module      : Serialize
Description : POP3 reply serialization
Portability : POSIX

Defines the 'Serialize' type class and it's implementation for 'Reply',
which processes it into a 'ByteString' so that it can be sent into the network.
-}
module Serialize
  ( -- * Class
    serialize
  ) where

import Data.List (dropWhileEnd)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, split)

import Session
  ( Reply (..)
  , PassReply (..)
  , StatReply (..)
  , ListEntry (..)
  , ListReply (..)
  , RetrReply (..)
  , DeleReply (..)
  , RsetReply (..)
  , QuitReply (..)
  , UidlEntry (..)
  , UidlReply (..)
  , SessionErr (..)
  )


-- Imports --------------------------------------------------------------

import Error (annotate, Oper (OpRead))
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (MonadIO(liftIO))


-- Syscalls -------------------------------------------------------------

-- | Annotated 'readFile' syscall which given
-- the path, loads its contents into a 'ByteString'
--
-- Throws 'SysErr' on failure.
tryReadFile :: FilePath -> IO ByteString
tryReadFile path = annotate OpRead call
  where call = BS.readFile path


-- Constants ------------------------------------------------------------

-- | All POP3 response lines must terminate with CRLF.
crlf :: ByteString
crlf = "\r\n"

-- | All POP3 positive responses must start with "+OK".
okInd :: ByteString
okInd = "+OK"

-- | All POP3 negative responses must start with "-ERR".
errInd :: ByteString
errInd = "-ERR"

-- | All POP3 multiline responses must finish with
-- the terminating octet "." followed by a CRLF.
term :: ByteString
term = "." <> crlf


-- Formatting -----------------------------------------------------------

-- | Formats the contents of a file following the POP3 protocol.
-- This is done by replacing all newlines by CRLF and by dot-stuffing,
-- which consists on escaping all lines which start with "." with another ".".
formatFile :: ByteString -> ByteString
formatFile = BS.intercalate "\r\n" . map (dotStuff . stripCR) . dropWhole . split '\n'
  where
    stripCR line = fromMaybe line (BS.stripSuffix "\r" line)
    dotStuff line
        | BS.isPrefixOf "." line = "." <> line
        | otherwise              = line
    dropWhole = dropWhileEnd BS.null


-- Type Class -----------------------------------------------------------

-- | Serializes a type into a 'ByteString'
-- so that it can be sent into the network.
class Serialize a where
  -- Processes 'a' into a 'ByteString'.
  -- Works with any monad with 'MonadIO'.
  serialize :: MonadIO f => a -> f ByteString


-- Reply Instance -------------------------------------------------------

-- | 'Reply' implementation of 'Serialize'.
instance Serialize Reply where
  serialize = \case
    RepHelo      -> pure $ okInd <> " POP3 server ready" <> crlf
    RepUser      -> pure $ okInd <> crlf
    RepPass pass -> serialize pass
    RepStat stat -> serialize stat
    RepList list -> serialize list
    RepRetr retr -> serialize retr
    RepDele dele -> serialize dele
    RepRset rset -> serialize rset
    RepNoop      -> pure $ okInd <> crlf
    RepUidl uidl -> serialize uidl
    RepQuit quit -> serialize quit
    RepExcp      -> pure $ errInd <> " " <> "internal failure"
    RepErr  err  -> serialize err

-- | Serializes the PASS command reply, with the format:
-- +OK <user>'s maildrop has <count> messages (<size> octets)
instance Serialize PassReply where
  serialize pass = pure $ okInd
    <> " "
    <> (pack . show . passUser) pass
    <> "'s maildrop has "
    <> (pack . show . passCount) pass
    <> " messages ("
    <> (pack . show . passSize) pass
    <> " octets)"
    <> crlf

-- | Serializes the STAT command reply, with the format:
-- +OK <count> <size>
instance Serialize StatReply where
  serialize stat = pure $ okInd
    <> " "
    <> (pack . show . statCount) stat
    <> " "
    <> (pack . show . statSize) stat
    <> crlf

-- | Serializes a LIST command line, with the format:
-- <num> <size>
instance Serialize ListEntry where
  serialize entry = pure $ ""
    <> (pack . show . listId) entry
    <> " "
    <> (pack . show . listSize) entry

-- | Serializes a LIST command reply, with the format:
-- +OK
-- 1 <size1>
-- ...
-- n <sizen>
-- .
instance Serialize ListReply where
  serialize = \case
    ListOne entry -> do
      s <- serialize entry
      pure $ okInd <> " " <> s <> crlf
    ListAll entries -> do
      serialized <- mapM serialize entries
      pure $ BS.intercalate crlf (okInd : serialized ++ [term])

-- | Serializes a RETR command reply, with the format:
-- +OK <size> octets
-- <dot-stuffed file contents>
-- .
instance Serialize RetrReply where
  serialize retr = liftIO $ do
    contents <- formatFile <$> tryReadFile (retrPath retr)

    let header = okInd
          <> " "
          <> (pack . show) (retrSize retr)
          <> " octets"

    pure $ if BS.null contents
      then header <> crlf <> term
      else BS.intercalate crlf [header, contents, term]
  
-- | Serializes a DELE command reply, with the format:
-- +OK message <id> deleted
instance Serialize DeleReply where
  serialize dele = pure $ okInd
    <> " message "
    <> (pack . show . deleId) dele
    <> " deleted"
    <> crlf

-- | Serializes a RSET command reply, with the format:
-- +OK restored <count> messages (<size> octets)
instance Serialize RsetReply where
  serialize rset = pure $ okInd
    <> " restored "
    <> (pack . show . rsetCount) rset
    <> " messages ("
    <> (pack . show . rsetSize) rset
    <> " octets)"
    <> crlf

-- | Serializes a UIDL command line, with the format:
-- <num> <uid>
instance Serialize UidlEntry where
  serialize entry = pure $ ""
    <> (pack . show . uidlId) entry
    <> " "
    <> (pack . show . uidlUID) entry

-- | Serializes a UIDL command reply, with the format:
-- +OK
-- 1 <uid1>
-- ...
-- n <uidn>
-- .
instance Serialize UidlReply where
  serialize = \case
    UidlOne entry -> do
      s <- serialize entry
      pure $ okInd <> " " <> s <> crlf
    UidlAll entries -> do
      serialized <- mapM serialize entries
      pure $ BS.intercalate crlf (okInd : serialized ++ [term])

-- | Serializes a QUIT command reply.
--
-- If the user didn't authenticate, the format is:
-- +OK <user> POP3 server signing off
--
-- If the user authenticated and his mailbox is empty, the format is:
-- +OK <user> POP3 server signing off (maildrop empty)
--
-- If the user authenticated and his mailbox isn't empty, the format is:
-- +OK <user> POP3 server signing off (<count> messages left)
instance Serialize QuitReply where
  serialize quit = pure $ case quitCount quit of
    Nothing -> okInd
      <> " POP3 server signing off"
      <> crlf
    Just 0 -> okInd
      <> " POP3 server signing off (maildrop empty)"
      <> crlf
    Just count -> okInd
      <> " POP3 server signing off ("
      <> (pack . show) count
      <> " messages left)"
      <> crlf

-- | Serializes an error reply, with the format:
-- -Err <msg>
instance Serialize SessionErr where
  serialize err = pure $ errInd
    <> " "
    <> (pack . show) err
    <> crlf
