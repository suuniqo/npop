{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : ParserSpec
Description : Hedgehog fuzzing tests for the Query module
Portability : POSIX

Generates valid and invalid queries using Hedgehog,
and tests parsing invariants and properties over them.
-}
module ParserSpec (spec) where


-- Imports --------------------------------------------------------------

import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import Data.Char
  ( toUpper
  , toLower
  , isAlpha
  , isSpace
  )

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog
    ( Gen
    , PropertyT
    , forAll
    , (===)
    , success
    , failure
    , annotate
    , assert
    )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

import Constant (queryMaxLen)
import Query (Query (..), parseQuery')
import Types (MsgNo, readMsgNo)


-- Generators Valid -----------------------------------------------------

-- | Generates random whitespace. Although strictly
-- POP3 should only allow a unique ' ' as a separator,
-- modern implementations are more flexible and allow
-- any separator as long as it's composed by whitespace.
genWhitespace :: Gen ByteString
genWhitespace = BS.pack
  <$> Gen.string (Range.linear 1 5) (Gen.element [' ', '\t'])

-- | Generates a random string. It must
-- be completely alphanumeric to protect
-- against prompt injection attacks.
genAtom :: Gen ByteString
genAtom = BS.pack
  <$> Gen.string (Range.linear 1 20) Gen.alphaNum

-- | Generates a random 'MsgNo'. It can
-- lie between 1 and maxBound :: Int.
genMsgNo :: Gen MsgNo
genMsgNo = do
    n <- Gen.int (Range.linear 1 (maxBound :: Int))
    let bs = BS.pack (show n)
    maybe Gen.discard pure (readMsgNo bs)

-- | Generates a random casing for the provided
-- ByteString, as POP3 commands are case insensitive.
genRandCase :: ByteString -> Gen ByteString
genRandCase bs = BS.pack <$> traverse cases (BS.unpack bs)
  where
    cases c
      | isAlpha c = Gen.element [toLower c, toUpper c]
      | otherwise = pure c

-- | Generates a valid POP3 query.
genQuery :: Gen Query
genQuery = Gen.choice
  [ User <$> genAtom
  , Pass <$> genAtom
  , pure Stat
  , Gen.choice
      [ pure (List Nothing)
      , List . Just <$> genMsgNo
      ]
  , Gen.choice
      [ pure (Uidl Nothing)
      , Uidl . Just <$> genMsgNo
      ]
  , Retr <$> genMsgNo
  , Dele <$> genMsgNo
  , pure Noop
  , pure Rset
  , pure Quit
  ]


-- Generators Invalid ---------------------------------------------------

-- | Maximum 'Int' as an 'Integer'.
maxInt :: Integer
maxInt = fromIntegral (maxBound :: Int)

-- | Generates a number smaller or equal
-- to zero, which is always invalid.
genNegNum :: Gen ByteString
genNegNum = BS.pack . show
  <$> Gen.int (Range.linear (minBound :: Int) 0)

-- | Generates a number bigger than the max bound.
genOvfNum :: Gen ByteString
genOvfNum = BS.pack . show
  <$> Gen.integral (Range.linear (maxInt + 1) (2 * maxInt))

-- | Generates a random alphabetical string.
genAlpha :: Gen ByteString
genAlpha =
  BS.pack <$> Gen.string
    (Range.linear 1 20)
    Gen.alpha

-- | Generates a long alphanumeric string.
genBigAtom :: Gen ByteString
genBigAtom = BS.pack
  <$> Gen.string (Range.linear 0 queryMaxLen) Gen.ascii

-- | Generates an invalid POP3 query.
genInvalidQuery :: Gen ByteString
genInvalidQuery =
  Gen.choice
    [ pure "FOO"
    , pure "BAR"

    , pure "STAT 1 2"
    , ("STAT " <>) <$> genAtom

    , pure "RSET 1 2"
    , ("RSET " <>) <$> genAtom

    , pure "QUIT 1 2"
    , ("QUIT " <>) <$> genAtom

    , pure "RETR"
    , ("RETR " <>)   <$> genNegNum
    , ("RETR " <>)   <$> genOvfNum
    , ("RETR " <>)   <$> genAlpha
    , ("RETR 1 " <>) <$> genAtom

    , pure "DELE"
    , ("DELE " <>)   <$> genNegNum
    , ("DELE " <>)   <$> genOvfNum
    , ("DELE " <>)   <$> genAlpha
    , ("DELE 1 " <>) <$> genAtom

    , ("LIST " <>)   <$> genAlpha
    , ("LIST " <>)   <$> genNegNum
    , ("LIST " <>)   <$> genOvfNum
    , ("LIST 1 " <>) <$> genAtom

    , ("UIDL " <>)   <$> genAlpha
    , ("UIDL " <>)   <$> genNegNum
    , ("UIDL " <>)   <$> genOvfNum
    , ("UIDL 1 " <>) <$> genAtom

    , pure "PASS"
    , pure "USER"

    , genBigAtom
    ]


-- Rendering ------------------------------------------------------------

-- | Converts a Query back into a valid POP3 command.
renderQuery :: Query -> ByteString
renderQuery (User user)     = "USER " <> user
renderQuery (Pass pass)     = "PASS " <> pass
renderQuery  Stat           = "STAT"
renderQuery (List Nothing)  = "LIST"
renderQuery (List (Just n)) = "LIST " <> BS.pack (show n)
renderQuery (Uidl Nothing)  = "UIDL"
renderQuery (Uidl (Just n)) = "UIDL " <> BS.pack (show n)
renderQuery (Retr n)        = "RETR " <> BS.pack (show n)
renderQuery (Dele n)        = "DELE " <> BS.pack (show n)
renderQuery  Noop           = "NOOP"
renderQuery  Rset           = "RSET"
renderQuery  Quit           = "QUIT"


-- Properties -----------------------------------------------------------

-- | Every rendered query must be parsed successfully.
prop_roundtrip :: PropertyT IO ()
prop_roundtrip = do
  query <- forAll genQuery
  parseQuery' (renderQuery query) === Right query

-- | Every rendered query where the command has
-- arbitrarily casing must be parsed successfully.
prop_caseInsensitive :: PropertyT IO ()
prop_caseInsensitive = do
  query <- forAll genQuery
  let (cmd, rest) = BS.break isSpace (renderQuery query)

  cmd' <- forAll (genRandCase cmd)
  let mutated = cmd' <> rest

  parseQuery' mutated === Right query

-- | Every rendered query where the all the tokens
-- are spaced arbitrarily must be parsed successfully.
prop_spaceInsensitive :: PropertyT IO ()
prop_spaceInsensitive = do
  query <- forAll genQuery

  ws1 <- forAll genWhitespace
  ws2 <- forAll genWhitespace
  ws3 <- forAll genWhitespace

  let tokens = BS.words $ renderQuery query

  let spaced = ws1 <> BS.intercalate ws2 tokens <> ws3

  parseQuery' spaced === Right query

-- | Invalid queries must always
-- fail and produce a 'QueryErr'.
prop_invalidQueriesFail :: PropertyT IO ()
prop_invalidQueriesFail = do
  query <- forAll genInvalidQuery
  case parseQuery' query of
    Left _  -> success
    Right q -> do
      annotate ("Unexpected success: " ++ show q)
      failure

-- | Rendered queries never exceed
-- the protocol query length limit.
prop_queryLengthLimit :: PropertyT IO ()
prop_queryLengthLimit = do
  query <- forAll genQuery
  assert $ BS.length (renderQuery query) <= queryMaxLen


-- Parser Spec ----------------------------------------------------------

-- | Executes and validates all
-- defined hedgehog properties.
spec :: Spec
spec = describe "Parser" $ do

  describe "valid queries" $ do
    it "roundtrip: render then parse gives same query" $
      hedgehog prop_roundtrip
    it "commands are case insensitive" $
      hedgehog prop_caseInsensitive
    it "surrounding whitespace is ignored" $
      hedgehog prop_spaceInsensitive
    it "rendered queries never exceed the length limit" $
      hedgehog prop_queryLengthLimit

  describe "invalid queries" $ do
    it "always produce a parse error" $
      hedgehog prop_invalidQueriesFail
