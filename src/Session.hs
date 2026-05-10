{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Session
  ( SessionErr (..)
  , Reply
  , startSession
  , processQuery
  , withAuth
  , finishSession
  ) where

import Data.Foldable (toList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Query (Query (..), QueryErr, MsgNo, toIdx, msgEnum)

import Storage
  ( Message (..)
  , Username
  , userValidate
  , StorageErr
  , UID
  , withLock
  , fetchMailbox
  , Lock
  , Flag (..)
  , updateMailbox
  )
import Data.ByteString (ByteString)
import Error (SysErr)
import Data.Maybe (isNothing)

-- Data

data StatReply = StatReply
  { statCount :: Int
  , statSize  :: Integer
  }

data ListEntry = ListEntry
  { listId   :: MsgNo
  , listSize :: Integer
  }

data ListReply
  = ListOne ListEntry
  | ListAll [ListEntry]

data UidlEntry = UidlEntry
  { uidlId  :: MsgNo
  , uidlUID :: UID
  }

data UidlReply
  = UidlOne UidlEntry
  | UidlAll [UidlEntry]

data QuitReply = QuitReply
  { quitUser :: Maybe Username 
  , quitLeft :: Maybe Int 
  }

data Reply
  = RepUser Username
  | RepPass Username
  | RepStat StatReply
  | RepList ListReply
  | RepRetr FilePath
  | RepDele MsgNo
  | RepRset Int
  | RepNoop
  | RepUidl UidlReply
  | RepQuit QuitReply

data SessionErr
  = Sys SysErr          -- TODO: handle correctly
  | Query QueryErr
  | Storage StorageErr
  | InvalidPhase
  | UserFirst
  | AlreadyDele
  | NoSuchMsg

-- Phases

data Auth
data Authed
data Trans
data Update

data Phase s where
  AuthPhase   :: Maybe Username -> Phase Auth
  AuthedPhase :: Username -> Phase Authed
  TransPhase  :: Username -> Lock -> Seq Message -> Set MsgNo -> Phase Trans
  UpdatePhase :: Username -> Lock -> Seq Message -> Set MsgNo -> Phase Update

data Transition s
  = Stay (Phase s)
  | Next (Phase (Next s))
  | Term

type PhaseResult a = Either SessionErr (Transition a, Reply)

class Process s where
  type Next s

  processQuery :: Phase s -> Either QueryErr Query -> PhaseResult s
  processQuery session query =
    case query of
      Left err -> Left (Query err)
      Right ok -> process session ok

  process :: Phase s -> Query -> PhaseResult s

-- Authentication Phase

startSession :: Phase Auth
startSession = AuthPhase Nothing

processUser :: Phase Auth -> ByteString -> PhaseResult Auth
processUser (AuthPhase _) user = case userValidate user of
  Right user' -> Right
    ( Stay $ AuthPhase (Just user')
    , RepUser user'
    )
  Left err -> Left $ Storage err

processPass :: Phase Auth -> ByteString -> PhaseResult Auth
processPass (AuthPhase maybeUser) _ = case maybeUser of
  Nothing   -> Left UserFirst
  Just user -> Right (Next (AuthedPhase user), RepPass user)    -- TODO: handle correctly

processQuitAuth :: Phase Auth -> PhaseResult Auth
processQuitAuth (AuthPhase user) = Right (Term, RepQuit $ QuitReply user Nothing)

instance Process Auth where
  type Next Auth = Authed

  process phase query = case query of
    User name -> processUser phase name
    Pass pass -> processPass phase pass
    Quit      -> processQuitAuth phase
    _         -> Left InvalidPhase

-- Transaction Phase

wrapLock :: Username -> (Phase Trans -> IO a) -> Lock -> IO (Either SessionErr a)
wrapLock user action lock = do
  maildrop <- fetchMailbox lock user

  case maildrop of
    Left err   -> pure $ Left (Storage err)
    Right msgs -> Right <$> action (TransPhase user lock msgs Set.empty)

withAuth :: Phase Authed -> (Phase Trans -> IO a) -> IO (Either SessionErr a)
withAuth (AuthedPhase user) action = do
  result <- withLock user (wrapLock user action)

  case result of
    Left err  -> pure $ Left (Storage err)
    Right res -> pure res

visible :: Seq Message -> Set MsgNo -> [(MsgNo, Message)]
visible msgs dels =
  [ (num, msg)
  | (num, msg) <- zip msgEnum (toList msgs)
  , not $ Set.member num dels
  ]

msgFetch :: MsgNo -> Seq Message -> Set MsgNo -> Maybe Message
msgFetch num msgs dels
  | Set.member num dels = Nothing
  | otherwise           = Seq.lookup (toIdx num) msgs

processStat :: Phase Trans -> PhaseResult Trans
processStat phase@(TransPhase _ _ msgs dels) =
  let leftMsgs = visible msgs dels
      count    = Seq.length msgs - Set.size dels
      sizeSum  = sum $ msgSize . snd <$> leftMsgs
  in Right
    ( Stay phase
    , RepStat $ StatReply count sizeSum
    )

buildListEntry :: (MsgNo, Message) -> ListEntry
buildListEntry (num, msg) = ListEntry num (msgSize msg)

processListOne :: Phase Trans -> MsgNo -> PhaseResult Trans
processListOne phase@(TransPhase _ _ msgs dels) num =
  case msgFetch num msgs dels of
    Nothing  -> Left NoSuchMsg
    Just msg -> Right
      ( Stay phase
      , RepList . ListOne $ buildListEntry (num, msg)
      )

processListAll :: Phase Trans -> PhaseResult Trans
processListAll phase@(TransPhase _ _ msgs dels) = Right
  ( Stay phase
  , RepList . ListAll $ map buildListEntry (visible msgs dels)
  )

buildUidlEntry :: (MsgNo, Message) -> UidlEntry
buildUidlEntry (num, msg) = UidlEntry num (msgUid msg)

processUidlOne :: Phase Trans -> MsgNo -> PhaseResult Trans
processUidlOne phase@(TransPhase _ _ msgs dels) num =
  case msgFetch num msgs dels of
    Nothing  -> Left NoSuchMsg
    Just msg -> Right
      ( Stay phase
      , RepUidl . UidlOne $ buildUidlEntry (num, msg)
      )

processUidlAll :: Phase Trans -> PhaseResult Trans
processUidlAll phase@(TransPhase _ _ msgs dels) = Right
  ( Stay phase
  , RepUidl . UidlAll $ map buildUidlEntry (visible msgs dels)
  )

processRetr :: Phase Trans -> MsgNo -> PhaseResult Trans
processRetr (TransPhase user lock msgs dels) num = 
  case msgFetch num msgs dels of
    Nothing  -> Left NoSuchMsg
    Just msg -> do
      let newFlags = Set.insert Seen (msgFlags msg)
      let newMsgs  = Seq.adjust' (\m -> m { msgFlags = newFlags }) (toIdx num) msgs

      pure
        ( Stay (TransPhase user lock newMsgs dels)
        , RepRetr (msgPath msg)
        )
  
processDele :: Phase Trans -> MsgNo -> PhaseResult Trans
processDele (TransPhase user lock msgs dels) num
  | Set.member num dels                = Left AlreadyDele
  | isNothing (msgFetch num msgs dels) = Left NoSuchMsg
  | otherwise = Right
      ( Stay (TransPhase user lock msgs (Set.insert num dels))
      , RepDele num
      )

processRset :: Phase Trans -> PhaseResult Trans
processRset (TransPhase user lock msgs dels) = Right
  ( Stay (TransPhase user lock msgs Set.empty)
  , RepRset (Set.size dels)
  )

processNoop :: Phase Trans -> PhaseResult Trans
processNoop phase = Right (Stay phase, RepNoop)

processQuitTrans :: Phase Trans -> PhaseResult Trans
processQuitTrans (TransPhase user lock msgs dels) = Right
  ( Next (UpdatePhase user lock msgs dels)
  , RepQuit $ QuitReply (Just user) (Just $ Seq.length msgs - Set.size dels)
  )

instance Process Trans where
  type Next Trans = Update

  process phase query = case query of
    Stat            -> processStat phase
    List (Just num) -> processListOne phase num
    List Nothing    -> processListAll phase 
    Uidl (Just num) -> processUidlOne phase num
    Uidl Nothing    -> processUidlAll phase
    Retr num        -> processRetr phase num
    Dele num        -> processDele phase num
    Rset            -> processRset phase
    Noop            -> processNoop phase
    Quit            -> processQuitTrans phase
    _               -> Left InvalidPhase

finishSession :: Phase Update -> IO ()
finishSession (UpdatePhase user lock msgs dels) = updateMailbox lock user trash keep
  where
    trash = [msg | (msg, num) <- zip (toList msgs) msgEnum, Set.member    num dels]
    keep  = [msg | (msg, num) <- zip (toList msgs) msgEnum, Set.notMember num dels]
