{-# LANGUAGE OverloadedStrings #-}

module Serialize where

import Data.List (dropWhileEnd)
import Data.ByteString (ByteString, intercalate)
import Data.ByteString.Char8 (pack, split)
import qualified Data.ByteString as BS

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

import Error (annotate, Oper (OpRead))
import Data.Maybe (fromMaybe)

-- Syscalls

tryReadFile :: FilePath -> IO ByteString
tryReadFile path = annotate OpRead call
  where call = BS.readFile path

-- Constants

crlf :: ByteString
crlf = "\r\n"

ok :: ByteString
ok = "+OK"

okReply :: ByteString
okReply = ok <> crlf

term :: ByteString
term = "." <> crlf

err :: ByteString
err = "-ERR"

-- Formatting

formatFile :: ByteString -> ByteString
formatFile = intercalate "\r\n" . map (dotStuff . stripCR) . dropWhole . split '\n'
  where
    stripCR line = fromMaybe line (BS.stripSuffix "\r" line)
    dotStuff line
        | BS.isPrefixOf "." line = "." <> line
        | otherwise              = line
    dropWhole = dropWhileEnd BS.null

-- Serialize

class Serialize a where
  serialize :: a -> IO ByteString

instance Serialize Reply where
  serialize reply = case reply of
    RepHelo      -> pure $ ok <> " POP3 server ready" <> crlf
    RepUser      -> pure okReply
    RepPass pass -> serialize pass
    RepStat stat -> serialize stat
    RepList list -> serialize list
    RepRetr retr -> serialize retr
    RepDele dele -> serialize dele
    RepRset rset -> serialize rset
    RepNoop      -> pure okReply
    RepUidl uidl -> serialize uidl
    RepQuit quit -> serialize quit
    RepErr  erro -> serialize erro

-- +OK <user>'s maildrop has <count> messages (<size> octets)
instance Serialize PassReply where
  serialize pass = pure $ ok
    <> " "
    <> (pack . show . passUser) pass
    <> "'s maildrop has "
    <> (pack . show . passCount) pass
    <> " messages ("
    <> (pack . show . passSize) pass
    <> " octets)"
    <> crlf

-- +OK nn mm
instance Serialize StatReply where
  serialize stat = pure $ ok
    <> " "
    <> (pack . show . statCount) stat
    <> " "
    <> (pack . show . statSize) stat
    <> crlf

-- nn mm
instance Serialize ListEntry where
  serialize entry = pure $ ""
    <> (pack . show . listId) entry
    <> " "
    <> (pack . show . listSize) entry

-- +OK nn mm
-- +OK <count> messages (<size> octets)
-- n1 m1
-- ...
-- nk mk
-- .
instance Serialize ListReply where
  serialize list = case list of
    ListOne entry -> do
      s <- serialize entry
      pure $ ok <> " " <> s <> crlf
    ListAll entries -> do
      serialized <- mapM serialize entries
      let count  = length entries
          size   = sum (map listSize entries)
          header = ok
            <> " "
            <> (pack . show) count
            <> " messages ("
            <> (pack . show) size
            <> " octets)"
      pure $ intercalate crlf (header : serialized ++ [term])

-- +OK <size> octets
-- <file contents>
-- .
instance Serialize RetrReply where
  serialize retr = do
    contents <- formatFile <$> tryReadFile (retrPath retr)

    let header = ok
          <> " "
          <> (pack . show) (retrSize retr)
          <> " octets"

    pure $ if BS.null contents
      then header <> crlf <> term
      else intercalate crlf [header, contents, term]
  
-- +OK message <id> deleted
instance Serialize DeleReply where
  serialize dele = pure $ ok
    <> " message "
    <> (pack . show . deleId) dele
    <> " deleted"
    <> crlf

-- +OK restored <count> messages (<size> octets)
instance Serialize RsetReply where
  serialize rset = pure $ ok
    <> " restored "
    <> (pack . show . rsetCount) rset
    <> " messages ("
    <> (pack . show . rsetSize) rset
    <> " octets)"
    <> crlf

-- nn uu
instance Serialize UidlEntry where
  serialize entry = pure $ ""
    <> (pack . show . uidlId) entry
    <> " "
    <> (pack . show . uidlUID) entry

-- +OK nn uu
-- +OK
-- n1 u1
-- ...
-- nk uk
-- .
instance Serialize UidlReply where
  serialize list = case list of
    UidlOne entry -> do
      s <- serialize entry
      pure $ ok <> " " <> s <> crlf
    UidlAll entries -> do
      serialized <- mapM serialize entries
      pure $ intercalate crlf (ok : serialized ++ [term])

-- +OK <user> POP3 server signing off
-- +OK <user> POP3 server signing off (maildrop empty)
-- +OK <user> POP3 server signing off (<count> messages left)
instance Serialize QuitReply where
  serialize quit = pure $ case quitCount quit of
    Nothing -> ok
      <> " POP3 server signing off"
      <> crlf
    Just 0 -> ok
      <> " POP3 server signing off (maildrop empty)"
      <> crlf
    Just count -> ok
      <> " POP3 server signing off ("
      <> (pack . show) count
      <> " messages left)"
      <> crlf

-- -Err <msg>
instance Serialize SessionErr where
  serialize erro = pure $ err
    <> " "
    <> (pack . show) erro
    <> crlf
