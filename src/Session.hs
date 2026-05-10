{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Session
  ( SessionErr (..)
  , Reply (..)
  , PassReply (..)
  , StatReply (..)
  , ListEntry (..)
  , ListReply (..)
  , RetrReply (..)
  , DeleReply (..)
  , RsetReply (..)
  , UidlEntry (..)
  , UidlReply (..)
  , QuitReply (..)
  , Phase
  , Auth
  , Authed
  , Trans
  , Update
  , Transition (..)
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

import Query (Query (..), QueryErr (Client))

import Storage
  ( Message (..)
  , StorageErr
  , withLock
  , fetchMailbox
  , Lock
  , Flag (..)
  , updateMailbox
  )
import Data.ByteString (ByteString)
import Data.Maybe (isNothing)
import Error (SysErr)
import Server (ClientErr(Disconn, Timeout))

import Types
  ( UID
  , Username, userValidate
  , MsgNo, toIdx, msgEnum
  )

-- Data

data PassReply = PassReply
  { passUser  :: Username
  , passCount :: Int
  , passSize  :: Integer
  }
  deriving Show

data StatReply = StatReply
  { statCount :: Int
  , statSize  :: Integer
  }
  deriving Show

data ListEntry = ListEntry
  { listId   :: MsgNo
  , listSize :: Integer
  }
  deriving Show

data ListReply
  = ListOne ListEntry
  | ListAll [ListEntry]
  deriving Show

data UidlEntry = UidlEntry
  { uidlId  :: MsgNo
  , uidlUID :: UID
  }
  deriving Show

data UidlReply
  = UidlOne UidlEntry
  | UidlAll [UidlEntry]
  deriving Show

data RetrReply = RetrReply
  { retrPath :: FilePath
  , retrSize :: Integer
  }
  deriving Show

newtype DeleReply = DeleReply { deleId :: MsgNo }
  deriving Show

data RsetReply = RsetReply
  { rsetCount :: Int
  , rsetSize  :: Integer
  }
  deriving Show

newtype QuitReply = QuitReply { quitCount :: Maybe Int }
  deriving Show

data SessionErr
  = Sys SysErr  -- TODO: handle correctly
  | Query QueryErr
  | Storage StorageErr
  | InvalidPhase
  | InvalidUser
  | UserFirst
  | AlreadyDele
  | NoSuchMsg

instance Show SessionErr where
  show err = case err of
    Sys     _    -> "internal failure"
    Query   err' -> show err'
    Storage err' -> show err'
    InvalidPhase -> "command not available in this state"
    InvalidUser  -> "invalid username"
    UserFirst    -> "PASS requires USER first"
    AlreadyDele  -> "message already deleted"
    NoSuchMsg    -> "no such message"

data Reply
  = RepHelo
  | RepUser
  | RepPass PassReply
  | RepStat StatReply
  | RepList ListReply
  | RepRetr RetrReply
  | RepDele DeleReply
  | RepRset RsetReply
  | RepNoop
  | RepUidl UidlReply
  | RepQuit QuitReply
  | RepErr SessionErr
  deriving Show

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
  = Stay (Phase s, Reply)
  | Next (Phase (Next s))
  | Term (Maybe Reply)

type PhaseResult a = Transition a

class Process s where
  type Next s

  processQuery :: Phase s -> Either QueryErr Query -> PhaseResult s
  processQuery session query =
    case query of
      Right ok -> process session ok
      Left err -> case err of
        Client Disconn -> Term Nothing
        Client Timeout -> Term . Just . RepErr . Query $ Client Disconn
        _ -> Stay (session, RepErr $ Query err)

  process :: Phase s -> Query -> PhaseResult s

-- Authentication Phase

startSession :: (Phase Auth, Reply)
startSession = (AuthPhase Nothing, RepHelo)

processUser :: Phase Auth -> ByteString -> PhaseResult Auth
processUser phase@(AuthPhase _) user = case userValidate user of
  Just user' -> Stay (AuthPhase $ Just user', RepUser)
  Nothing    -> Stay (phase, RepErr InvalidUser)

processPass :: Phase Auth -> ByteString -> PhaseResult Auth
processPass phase@(AuthPhase maybeUser) _ = case maybeUser of
  Nothing   -> Stay (phase, RepErr UserFirst)
  Just user -> Next (AuthedPhase user)

processQuitAuth :: Phase Auth -> PhaseResult Auth
processQuitAuth _ = Term $ Just (RepQuit $ QuitReply Nothing)

instance Process Auth where
  type Next Auth = Authed

  process phase query = case query of
    User name -> processUser phase name
    Pass pass -> processPass phase pass
    Quit      -> processQuitAuth phase
    _         -> Stay (phase, RepErr InvalidPhase)

-- Transaction Phase

wrapLock :: Username -> (Phase Trans -> Reply -> IO ()) -> Lock -> IO ()
wrapLock user action lock = do
  msgs <- fetchMailbox lock user

  let count = Seq.length msgs
  let size  = sum $ msgSize <$> msgs

  action (TransPhase user lock msgs Set.empty) (RepPass $ PassReply user count size)

withAuth :: Phase Authed -> (Phase Trans -> Reply -> IO ()) -> IO (Maybe Reply)
withAuth (AuthedPhase user) action = do
  result <- withLock user (wrapLock user action)

  pure $ case result of
    Left err -> Just (RepErr $ Storage err)
    Right () -> Nothing

trash :: Seq Message -> Set MsgNo -> [Message]
trash msgs dels = [msg | (msg, num) <- zip (toList msgs) msgEnum, Set.member num dels]

seen :: Seq Message -> [Message]
seen msgs = [msg | msg <- toList msgs, Set.member Seen (msgFlags msg)]

keepView :: Seq Message -> Set MsgNo -> [(MsgNo, Message)]
keepView msgs dels =
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
  let leftMsgs = keepView msgs dels
      count    = Seq.length msgs - Set.size dels
      sizeSum  = sum $ msgSize . snd <$> leftMsgs
  in Stay 
    ( phase
    , RepStat $ StatReply count sizeSum
    )

buildListEntry :: (MsgNo, Message) -> ListEntry
buildListEntry (num, msg) = ListEntry num (msgSize msg)

processListOne :: Phase Trans -> MsgNo -> PhaseResult Trans
processListOne phase@(TransPhase _ _ msgs dels) num =
  case msgFetch num msgs dels of
    Nothing  -> Stay (phase, RepErr NoSuchMsg)
    Just msg -> Stay 
      ( phase
      , RepList . ListOne $ buildListEntry (num, msg)
      )

processListAll :: Phase Trans -> PhaseResult Trans
processListAll phase@(TransPhase _ _ msgs dels) = Stay 
  ( phase
  , RepList . ListAll $ map buildListEntry (keepView msgs dels)
  )

buildUidlEntry :: (MsgNo, Message) -> UidlEntry
buildUidlEntry (num, msg) = UidlEntry num (msgUid msg)

processUidlOne :: Phase Trans -> MsgNo -> PhaseResult Trans
processUidlOne phase@(TransPhase _ _ msgs dels) num =
  case msgFetch num msgs dels of
    Nothing  -> Stay (phase, RepErr NoSuchMsg)
    Just msg -> Stay
      ( phase
      , RepUidl . UidlOne $ buildUidlEntry (num, msg)
      )

processUidlAll :: Phase Trans -> PhaseResult Trans
processUidlAll phase@(TransPhase _ _ msgs dels) = Stay 
  ( phase
  , RepUidl . UidlAll $ map buildUidlEntry (keepView msgs dels)
  )

processRetr :: Phase Trans -> MsgNo -> PhaseResult Trans
processRetr phase@(TransPhase user lock msgs dels) num = 
  case msgFetch num msgs dels of
    Nothing  -> Stay (phase, RepErr NoSuchMsg)
    Just msg -> 
      let newFlags = Set.insert Seen (msgFlags msg)
          newMsgs  = Seq.adjust' (\m -> m { msgFlags = newFlags }) (toIdx num) msgs
      in Stay 
        ( TransPhase user lock newMsgs dels
        , RepRetr $ RetrReply (msgPath msg) (msgSize msg)
        )
  
processDele :: Phase Trans -> MsgNo -> PhaseResult Trans
processDele phase@(TransPhase user lock msgs dels) num
  | Set.member num dels                = Stay (phase, RepErr AlreadyDele)
  | isNothing (msgFetch num msgs dels) = Stay (phase, RepErr NoSuchMsg)
  | otherwise = Stay
      ( TransPhase user lock msgs (Set.insert num dels)
      , RepDele $ DeleReply num
      )

processRset :: Phase Trans -> PhaseResult Trans
processRset (TransPhase user lock msgs dels) = Stay
  ( TransPhase user lock msgs Set.empty
  , RepRset $ RsetReply (Set.size dels) (sum $ msgSize <$> trash msgs dels)
  )

processNoop :: Phase Trans -> PhaseResult Trans
processNoop phase = Stay (phase, RepNoop)

processQuitTrans :: Phase Trans -> PhaseResult Trans
processQuitTrans (TransPhase user lock msgs dels) =
  Next (UpdatePhase user lock msgs dels)

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
    _               -> Stay (phase, RepErr InvalidPhase)

-- Update Phase

finishSession :: Phase Update -> IO Reply
finishSession (UpdatePhase user lock msgs dels) = do
  updateMailbox lock user (trash msgs dels) (seen msgs)
  pure $ RepQuit $ QuitReply (Just $ Seq.length msgs - Set.size dels)
