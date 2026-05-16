{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

{- |
Module      : Session
Description : Session state machine and command execution
Portability : POSIX

Maintains the session state for a single POP3 connection, including
authentication status and current mailbox view, and consumes parsed
commands, applying them to the mailbox and saving all changes by the end.
-}
module Session
  ( -- * Replies
    SessionErr (..)
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
  , excepReply
    -- * Phases
  , PhaseTag (..)
  , Phase
  , Transition (..)
    -- * Operations
  , startSession
  , processQuery
  , withAuth
  , finishSession
  ) where


-- Imports --------------------------------------------------------------

import Control.Monad.RWS (asks)
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)

import App (App, AppEnv (shadow))
import Query (Query (..), QueryErr (Client))
import Server (ClientErr(Disconn, Timeout))
import Shadow (auth)
import Storage
  ( StorageErr
  , Lock
  , withLock
  , fetchMailbox
  , updateMailbox
  )
import Types
  ( UID
  , Username
  , userValidate
  , MsgNo
  , toIdx
  , msgEnum
  , Flag (..)
  , Message (..)
  , hasFlag
  , addFlag
  )


-- Maildrop -------------------------------------------------------------

-- | Encodes the successfully acquired maildrop of a user.
data Maildrop = Maildrop
  { mdrpLock :: Lock            -- ^ Exclusive lock over the maildrop
  , mdrpMsgs :: Seq Message     -- ^ Maildrop messages
  , mdrpDels :: Set MsgNo       -- ^ Messages marked as deleted
  }

-- | Returns whether the specified message has been deleted during the session.
deleted :: MsgNo -> Maildrop -> Bool 
deleted num maildrop = Set.member num (mdrpDels maildrop)

-- | Returns whether the specified message hasn't been deleted during the session.
notDeleted :: MsgNo -> Maildrop -> Bool 
notDeleted num = not . deleted num

-- | Returns all messages marked as deleted during the session in the provided 'Maildrop'.
msgsDeleted :: Maildrop -> [Message]
msgsDeleted maildrop = 
  let msgs = mdrpMsgs maildrop
  in [msg | (msg, num) <- zip (toList msgs) msgEnum, deleted num maildrop]

-- | Returns all messages that haven't been marked as deleted during the session, in the provided 'Maildrop'.
msgsLeft :: Maildrop -> [Message]
msgsLeft maildrop = 
  let msgs = mdrpMsgs maildrop
  in [msg | (num, msg) <- zip msgEnum (toList msgs), notDeleted num maildrop]

-- | Returns all messages marked with the flag 'Seen' and which
-- haven't been deleted during the session, in the provided 'Maildrop'.
msgsSeen :: Maildrop -> [Message]
msgsSeen maildrop = [msg | msg <- msgsLeft maildrop, hasFlag Seen msg]

-- | Returns all messages in the provided 'Maildrop' which
-- haven't been marked as deleted during the session, along
-- with their respective POP3 indices.
msgsLeftView :: Maildrop -> [(MsgNo, Message)]
msgsLeftView maildrop =
  let msgs = mdrpMsgs maildrop
  in [(num, msg) | (num, msg) <- zip msgEnum (toList msgs), notDeleted num maildrop]

-- | Returns the number of messages which have been marked as
-- deleted during the session, in the provided 'Maildrop'.
deleCount :: Maildrop -> Int
deleCount maildrop = Set.size $ mdrpDels maildrop

-- | Returns the number of messages which haven't been marked as
-- deleted during the session, in the provided 'Maildrop'.
keepCount :: Maildrop -> Int
keepCount maildrop = Seq.length (mdrpMsgs maildrop) - deleCount maildrop

-- | Given a POP3 message number, tries to fetch it from the provided 'Maildrop'. 
--
-- It succeeds if the message is present in the original maildrop, and it hasn't
-- been marked as deleted during the session. Otherwise 'Nothing' is returned.
msgFetch :: MsgNo -> Maildrop -> Maybe Message
msgFetch num maildrop
  | Set.member num dels = Nothing
  | otherwise           = Seq.lookup (toIdx num) msgs
  where
    msgs = mdrpMsgs maildrop
    dels = mdrpDels maildrop

-- | Given a new message, it substitutes the message at position 'num' in the provided 'Maildrop'.
msgAdjust :: MsgNo -> Message -> Maildrop -> Maildrop
msgAdjust num msg maildrop =
  let newMsgs = Seq.adjust' (const msg) (toIdx num) (mdrpMsgs maildrop)
  in maildrop { mdrpMsgs = newMsgs }

-- | Given a message number, it is inserted in the deletion set
-- of the provided 'Maildrop' without doing any additional checks.
msgDelete :: MsgNo -> Maildrop -> Maildrop
msgDelete num maildrop =
  let dels = mdrpDels maildrop
  in maildrop { mdrpDels = Set.insert num dels }


-- Replies --------------------------------------------------------------

-- | Encodes a successful reply to the PASS command.
data PassReply = PassReply
  { passUser  :: Username   -- ^ Associated username
  , passCount :: Int        -- ^ Number of messages
  , passSize  :: Integer    -- ^ Maildrop size
  } deriving Show

-- | Encodes a successful reply to the STAT command.
data StatReply = StatReply
  { statCount :: Int        -- ^ Number of messages
  , statSize  :: Integer    -- ^ Maildrop size
  } deriving Show

-- | Encodes the scan listing of a single message,
-- used to build replies to the LIST command.
data ListEntry = ListEntry
  { listId   :: MsgNo       -- ^ Message number
  , listSize :: Integer     -- ^ Message size
  } deriving Show

-- | Encodes a successful reply to the LIST command.
data ListReply
  = ListOne ListEntry       -- ^ If the message is specified, only one scan listing
  | ListAll [ListEntry]     -- ^ If no message is specified, includes all scan listings
  deriving Show

-- | Encodes the unique-id listing of a single message,
-- used to build replies to the UIDL command.
data UidlEntry = UidlEntry
  { uidlId  :: MsgNo        -- ^ Message number
  , uidlUID :: UID          -- ^ Message unique-id
  } deriving Show

-- | Encodes a successful reply to the UIDL command.
data UidlReply
  = UidlOne UidlEntry       -- ^ If the message is specified, only one unique-id listing
  | UidlAll [UidlEntry]     -- ^ If no message is specified, includes all unique-id listings 
  deriving Show

-- | Encodes a successful reply to the RETR command.
data RetrReply = RetrReply
  { retrPath :: FilePath    -- ^ Message filepath
  , retrSize :: Integer     -- ^ Message size
  } deriving Show

-- | Encodes a successful reply to the DELE command.
newtype DeleReply = DeleReply { deleId :: MsgNo }
  deriving Show

-- | Encodes a successful reply to the RSET command.
data RsetReply = RsetReply
  { rsetCount :: Int        -- ^ Number of deleted messages
  , rsetSize  :: Integer    -- ^ Total size of deleted messages
  } deriving Show

-- | Encodes a successful reply to the QUIT command.
newtype QuitReply = QuitReply { quitCount :: Maybe Int }
  deriving Show

-- | Encodes a failure when processing a query.
data SessionErr
  = Query QueryErr          -- ^ Failed to load or pass the query
  | Storage StorageErr      -- ^ Failed to load or save messages on disk
  | InvalidPhase            -- ^ Command can't be used on current phase
  | InvalidUser             -- ^ Username format is incorrect
  | UserFirst               -- ^ PASS command issued without username
  | InvalidCreds            -- ^ Failed to authenticate user
  | AlreadyDele             -- ^ Tried to delete an already deleted message
  | NoSuchMsg               -- ^ Referenced a message that doesn't exist

instance Show SessionErr where
  show err = case err of
    Query   err' -> show err'
    Storage err' -> show err'
    InvalidPhase -> "command not available in this state"
    InvalidUser  -> "invalid username"
    UserFirst    -> "PASS requires USER first"
    InvalidCreds -> "invalid credentials"
    AlreadyDele  -> "message already deleted"
    NoSuchMsg    -> "no such message"

-- | Encodes a positive or negative reply to a query.
data Reply
  = RepHelo             -- ^ Initial greeting
  | RepUser             -- ^ USER command reply
  | RepPass PassReply   -- ^ PASS command reply
  | RepStat StatReply   -- ^ STAT command reply
  | RepList ListReply   -- ^ LIST command reply
  | RepRetr RetrReply   -- ^ RETR command reply
  | RepDele DeleReply   -- ^ DELE command reply
  | RepRset RsetReply   -- ^ RSET command reply
  | RepNoop             -- ^ NOOP command reply
  | RepUidl UidlReply   -- ^ UIDL command reply
  | RepQuit QuitReply   -- ^ QUIT command reply
  | RepExcp             -- ^ Message in case of an internal failure
  | RepErr SessionErr   -- ^ Negative reply in case of a 'SessionErr'
  deriving Show

-- | Returns the internal error reply.
-- It is always available to other modules.
excepReply :: Reply
excepReply = RepExcp


-- Phases ---------------------------------------------------------------

-- | Encodes the different states
-- a POP3 session can go through
data PhaseTag
  = Auth        -- ^ Authentication State
  | Verif       -- ^ Verification State
  | Trans       -- ^ Transaction State
  | Update      -- ^ Update State

-- | Encodes the state machine of a POP3 session,
-- where 's' is the current session state.
data Phase (s :: PhaseTag) where
  -- | Represents the session state during the AUTHENTICATION phase.
  --
  -- This is the initial phase of every POP3 session. The client must
  -- successfully identify itself before the server grants access to any
  -- maildrop resources.
  --
  -- The session advances to the TRANSACTION phase upon successful
  -- authentication. The client may also issue a QUIT command to end
  -- the session immediately without making any changes.
  --
  -- Fields:
  --   * @Maybe Username@: the username that may or may not have been sent yet.
  AuthPhase :: Maybe Username -> Phase Auth

  -- | Represents the intermediate state during credential verification.
  --
  -- This phase bridges AUTHENTICATION and TRANSACTION, by storing the
  -- needed data to verify the client's identity and acquire his maildrop.
  --
  -- On success, the server locks and loads the maildrop, advancing to
  -- 'TransPhase'. On failure, the session reverts to 'AuthPhase'.
  --
  -- Fields:
  --   * @Username@  : the name of the user attempting authentication.
  --   * @ByteString@: the supplied password or credential.
  VerifPhase :: Username -> ByteString -> Phase Verif

  -- | Represents the session state during the TRANSACTION phase.
  --
  -- Entered after acquiring the user's maildrop, 
  -- in this phase the user can request actions
  -- on part of the POP3 server, such as listing
  -- retrieving or deleting mail.
  --
  -- Once satisfied, the client sends the QUIT command
  -- and the session enters the UPDATE phase.
  --
  -- Fields:
  --   * @Username@   : the authenticated user who owns the maildrop.
  --   * @Lock@       : the exclusive lock held on the maildrop.
  --   * @Seq Message@: the ordered sequence of messages in the maildrop.
  --   * @Set MsgNo@  : the set of message numbers marked for deletion.
  TransPhase :: Username -> Maildrop -> Phase Trans

  -- | Represents the session state during the UPDATE phase.
  --
  -- During this final phase, the POP3 server releases any
  -- resources acquired during the TRANSACTION phase, and all
  -- changes are saved. The TCP connection is then closed.
  --
  -- Fields:
  --   * @Username@   : the authenticated user whose maildrop is being updated.
  --   * @Lock@       : the exclusive lock to be released on completion.
  --   * @Seq Message@: the ordered sequence of messages in the maildrop.
  --   * @Set MsgNo@  : the set of message numbers to be permanently removed.
  UpdatePhase :: Username -> Maildrop -> Phase Update

-- | Encodes a single step in the POP3 session state machine.
--
-- The type parameter 's' ties the transition to a
-- specific phase, ensuring that invalid states
-- are invalid at compile time.
data Transition (s :: PhaseTag)
  = Stay (Phase s) Reply    -- ^ Stays on the same phase and replies
  | Next (Phase (Next s))   -- ^ Advances to the next phase without reply
  | Term Reply              -- ^ Terminates a session with a reply
  | Abrt                    -- ^ Aborts a session without replies.

-- | Defines how a POP3 session 'Phase' processes queries.
class ProcessQuery (s :: PhaseTag) where
  -- | The phase that follows 's' upon a successful transition.
  type Next s :: PhaseTag

  -- | General @Either QueryErr Query@ processor which
  -- centralizes error handling in case of a @Left QueryErr@,
  -- delegating the processing of a correct 'Query' to 'process'.
  processQuery :: Phase s -> Either QueryErr Query -> Transition s
  processQuery session query =
    case query of
      Right ok -> process session ok                -- Correct queries are delegated to 'process'
      Left err -> case err of
        Client Disconn -> Abrt                      -- If the client disconnects, abort the session
        Client Timeout -> Abrt                      -- If the client times out, abort the session
        _ -> Stay session (RepErr $ Query err)      -- If another error occurs, emit a negative reply and continue

  
  -- | Processes a well-formed 'Query' into a 'Reply'
  -- and produces the next POP3 state transition.
  process :: Phase s -> Query -> Transition s


-- Authentication Phase -------------------------------------------------

-- | Entry point of the session state machine, providing an
-- empty @Phase Auth@, along with the initial POP3 server greeting.
startSession :: (Phase Auth, Reply)
startSession = (AuthPhase Nothing, RepHelo)

-- | Handles the USER command, given a @Phase Auth@. 
--
-- The username format is validated without checking
-- whether the username exists or not as to not reveal
-- information to potential attackers.
processUser :: Phase Auth -> ByteString -> Transition Auth
processUser phase@(AuthPhase _) user = case userValidate user of
  Just user' -> Stay (AuthPhase $ Just user') RepUser
  Nothing    -> Stay phase (RepErr InvalidUser)

-- | Handles the PASS command, given a @Phase Auth@. 
--
-- The commands requires that the username is already
-- specified, otherwise a negative response is emitted.
--
-- If the command succeeds, the session advances to @Phase Verif@,
-- where the provided credentials are validated.
processPass :: Phase Auth -> ByteString -> Transition Auth
processPass phase@(AuthPhase maybeUser) pass = case maybeUser of
  Nothing   -> Stay phase (RepErr UserFirst)
  Just user -> Next (VerifPhase user pass)

-- | Handles the QUIT command, given a @Phase Auth@. 
--
-- Quitting is always successful during the AUTHENTICATION
-- phase and it just ends the session without performing
-- any actions.
processQuitAuth :: Phase Auth -> Transition Auth
processQuitAuth _ = Term (RepQuit $ QuitReply Nothing)

-- | Processes well-formed queries during the AUTHENTICATION phase.
--
-- During this phase the only valid commands are USER, PASS and QUIT.
instance ProcessQuery Auth where
  type Next Auth = Verif             -- Next phase is 'Phase Verif'.

  process phase = \case
    User name -> processUser phase name                 -- Handles USER command
    Pass pass -> processPass phase pass                 -- Handles PASS command
    Quit      -> processQuitAuth phase                  -- Handles QUIT command
    _         -> Stay phase (RepErr InvalidPhase)       -- No other command is permitted


-- Verification Phase --------------------------------------------------

-- | Given a verified username, maps a function that needs a @Phase Trans@
-- and a reply to perform an 'App' action, into a function that needs a
-- lock to perform that same action.
--
-- Beginning the TRANSACTION phase needs exclusive access over the user's
-- mailbox, which in turns needs a 'Lock'. This wrapper given the necessary
-- lock, injects the @Phase Trans@ into the action that needs it.
wrapLock :: Username -> (Phase Trans -> Reply -> App ()) -> (Lock -> App ())
wrapLock user action lock = do
  msgs <- fetchMailbox lock user

  let count = Seq.length msgs
  let size  = sum $ msgSize <$> msgs

  action (TransPhase user (Maildrop lock msgs Set.empty)) (RepPass $ PassReply user count size)

-- Given a @Phase Verif@, with the client's credentials, tries
-- to authenticate the user and acquire exclusively his mailbox.
--
-- If it succeeds, it injects a @Phase Trans@ into the provided
-- action that needs it along with a reply to the PASS command,
-- and 'withAuth' itself returns 'Nothing'.
--
-- Otherwise a negative reply is returned along with a blank @Phase Auth@,
-- as now the client must repeat the AUTHENTICATION phase.
--
-- Additionally, all acquired resources are released, even on exceptions.
--
-- Throws 'SysErr' on failure.
withAuth :: Phase Verif -> (Phase Trans -> Reply -> App ()) -> App (Either (Phase Auth, Reply) ())
withAuth (VerifPhase user pass) action = do
  shdw  <- asks shadow

  if not $ auth shdw user pass
    then pure $ Left (AuthPhase Nothing, RepErr InvalidCreds)
    else do
      result <- withLock user (wrapLock user action)

      pure $ case result of
        Left err -> Left (AuthPhase Nothing, RepErr $ Storage err)
        Right () -> Right ()


-- Transaction Phase ----------------------------------------------------

-- | Handles the STAT command, given a @Phase Trans@. 
--
-- The STAT command returns a tuple containing the total number of non-deleted
-- messages and the sum of all of their sizes. It is always successful.
processStat :: Phase Trans -> Transition Trans
processStat phase@(TransPhase _ maildrop) =
  let leftMsgs = msgsLeft maildrop
      count    = keepCount maildrop
      sizeSum  = sum (msgSize <$> leftMsgs)
  in
    Stay phase (RepStat $ StatReply count sizeSum)

-- | Maps an enumerated message, to a 'ListEntry' containing it's number and size.
buildListEntry :: (MsgNo, Message) -> ListEntry
buildListEntry (num, msg) = ListEntry num (msgSize msg)

-- | Handles the LIST command when the message is specified, given a @Phase Trans@. 
--
-- In this case the LIST command returns a tuple containing the message
-- number and it's size. It can fail if there isn't a message with such number.
processListOne :: Phase Trans -> MsgNo -> Transition Trans
processListOne phase@(TransPhase _ maildrop) num =
  case msgFetch num maildrop of
    Nothing  -> Stay phase (RepErr NoSuchMsg)
    Just msg -> Stay phase (RepList . ListOne $ buildListEntry (num, msg))

-- | Handles the LIST command when no message is specified, given a @Phase Trans@. 
--
-- In this case the LIST command returns list of tuples containing the number and
-- size of all non-deleted messages in the maildrop. It is alway successful.
processListAll :: Phase Trans -> Transition Trans
processListAll phase@(TransPhase _ maildrop) =
  Stay phase (RepList . ListAll $ map buildListEntry (msgsLeftView maildrop))

-- | Handles the LIST command, given a @Phase Trans@. 
--
-- If no message is specified, it produces the scan listings of all messages.
-- Otherwise, it just produces the scan listing of the specified message.
processList :: Phase Trans -> Maybe MsgNo -> Transition Trans
processList phase Nothing    = processListAll phase
processList phase (Just num) = processListOne phase num

-- | Maps an enumerated message, to a 'UidlEntry' containing it's number and unique-id.
buildUidlEntry :: (MsgNo, Message) -> UidlEntry
buildUidlEntry (num, msg) = UidlEntry num (msgUid msg)

-- | Handles the UIDL command when the message is specified, given a @Phase Trans@. 
--
-- In this case the UIDL command returns a tuple containing the message
-- number and it's unique-id. It can fail if there isn't a message with such number.
processUidlOne :: Phase Trans -> MsgNo -> Transition Trans
processUidlOne phase@(TransPhase _ maildrop) num =
  case msgFetch num maildrop of
    Nothing  -> Stay phase (RepErr NoSuchMsg)
    Just msg -> Stay phase (RepUidl . UidlOne $ buildUidlEntry (num, msg))

-- | Handles the UIDL command when no message is specified, given a @Phase Trans@. 
--
-- In this case the UIDL command returns list of tuples containing the number and
-- unique-id of all non-deleted messages in the maildrop. It is alway successful.
processUidlAll :: Phase Trans -> Transition Trans
processUidlAll phase@(TransPhase _ maildrop) =
  Stay phase (RepUidl . UidlAll $ map buildUidlEntry (msgsLeftView maildrop))

-- | Handles the UIDL command, given a @Phase Trans@. 
--
-- If no message is specified, it produces the unique-id listings of all messages.
-- Otherwise, it just produces the unique-id listing of the specified message.
processUidl :: Phase Trans -> Maybe MsgNo -> Transition Trans
processUidl phase Nothing    = processUidlAll phase
processUidl phase (Just num) = processUidlOne phase num

-- | Handles the RETR command, given a @Phase Trans@.
--
-- The RETR command returns the contents of the message with the provided number.
-- However this function only sets the file path on it's reply, as the contents
-- are loaded lazily on serialization. Additionally, the selected message
-- is marked with the 'Seen' flag. The command is always succesful.
processRetr :: Phase Trans -> MsgNo -> Transition Trans
processRetr phase@(TransPhase user maildrop) num = 
  case msgFetch num maildrop of
    Nothing  -> Stay phase (RepErr NoSuchMsg)
    Just msg -> 
      let maildrop' = msgAdjust num (addFlag Seen msg) maildrop
      in Stay 
        (TransPhase user maildrop')
        (RepRetr $ RetrReply (msgPath msg) (msgSize msg))
  
-- | Handles the DELE command, given a @Phase Trans@.
--
-- The DELE command tries to delete the provided message by adding it
-- to the deletions set. However deleted messages are kept in the 'Sequence',
-- as they can be later restored by the RSET command. It can fail if there
-- isn't a message with such number, or if the message was already deleted.
processDele :: Phase Trans -> MsgNo -> Transition Trans
processDele phase@(TransPhase user maildrop) num
  | deleted num maildrop              = Stay phase (RepErr AlreadyDele)
  | isNothing (msgFetch num maildrop) = Stay phase (RepErr NoSuchMsg)
  | otherwise = Stay
      (TransPhase user (msgDelete num maildrop))
      (RepDele $ DeleReply num)

-- | Handles the RSET command, given a @Phase Trans@.
--
-- The RSET command restores all messages marked as deleted by just
-- emptying the deletion set. In this implementation, the message
-- also indicates the number of message restored as well as their
-- total size. It is always successful.
processRset :: Phase Trans -> Transition Trans
processRset (TransPhase user maildrop) = Stay
  (TransPhase user maildrop { mdrpDels = Set.empty })
  (RepRset $ RsetReply (deleCount maildrop) (sum $ msgSize <$> msgsDeleted maildrop))

-- | Handles the NOOP command, given a @Phase Trans@.
--
-- The NOOP command just produces a positive response, so it's always successful.
processNoop :: Phase Trans -> Transition Trans
processNoop phase = Stay phase RepNoop

-- | Handles the QUIT command, given a @Phase Trans@.
--
-- Quitting is always successful during the TRANSACTION phase.
-- The difference with quitting during the AUTHENTICATION phase
-- is that it triggers the UPDATE phase, where all changes are saved.
processQuitTrans :: Phase Trans -> Transition Trans
processQuitTrans (TransPhase user maildrop) =
  Next (UpdatePhase user maildrop)

-- | Processes well-formed queries during the TRANSACTION phase.
--
-- During this phase all commands are valid except USER and PASS.
instance ProcessQuery Trans where
  type Next Trans = Update             -- Next phase is 'Phase Update. 

  process phase = \case
    Stat            -> processStat phase                    -- Handles STAT command
    List maybeNum   -> processList phase maybeNum           -- Handles LIST command
    Uidl maybeNum   -> processUidl phase maybeNum           -- Handles UIDL command
    Retr num        -> processRetr phase num                -- Handles RETR command
    Dele num        -> processDele phase num                -- Handles DELE command
    Rset            -> processRset phase                    -- Handles RSET command
    Noop            -> processNoop phase                    -- Handles NOOP command
    Quit            -> processQuitTrans phase                -- Handles QUIT command
    _               -> Stay phase (RepErr InvalidPhase)     -- No other command is permitted


-- Update Phase -------------------------------------------------------

-- | Finishes a POP3 session by consuming a @Phase Update@,
-- saving all changes into storage, and returning one final reply. 
--
-- Throws 'SysErr' on failure.
finishSession :: Phase Update -> App Reply
finishSession (UpdatePhase user maildrop) = do
  updateMailbox (mdrpLock maildrop) user (msgsDeleted maildrop) (msgsSeen maildrop)
  pure $ RepQuit $ QuitReply $ Just $ keepCount maildrop
