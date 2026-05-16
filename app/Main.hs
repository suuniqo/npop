{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

{-|
Module      : Main
Description : Entry point for the POP3 server
Portability : POSIX

Runs the POP3 server, based on the app configuration.
-}
module Main where


-- Imports --------------------------------------------------------------

import Control.Monad (forever)
import Control.Exception (SomeException)
import qualified UnliftIO as UIO

import App (App, runApp, buildEnv, BuildErr (..))
import Log (emit, Severity(..))
import Query (buildQuery, Query, QueryErr)
import Server
  ( Connection
  , Listener
  , recvClient
  , sendClient
  , withListener
  , forkClient
  )
import Serialize (serialize)
import Session
  ( Phase
  , PhaseTag (..)
  , Reply
  , Transition (..)
  , excepReply
  , startSession
  , withAuth
  , finishSession
  , processQuery
  )


-- Error Handlers -------------------------------------------------------

-- | Reports an exception during listener acquisition.
onListenerErr :: SomeException -> App ()
onListenerErr err = emit Fail $ "failed to acquire listener: " <> show err

-- | Reports an exception during client acquisition.
onAcceptErr :: SomeException -> App ()
onAcceptErr err = emit Fail $ "failed when accepting client" <> show err

-- | Reports an exception during a client session,
-- and notifies the user by sending an error message.
onClientErr :: Connection -> SomeException -> App ()
onClientErr conn err = do
  sendReply conn excepReply
  emit Fail $ "failed while attending client" <> show err


-- Network Wrappers -----------------------------------------------------

-- | Sends a 'Reply' to the remote client.
sendReply :: Connection -> Reply -> App ()
sendReply conn reply = serialize reply >>= sendClient conn

-- | Receives a 'Query' from the remote client.
recvQuery :: Connection -> App (Either QueryErr Query)
recvQuery conn = buildQuery <$> recvClient conn


-- Session Logic --------------------------------------------------------

-- | Handles the UPDATE state by finishing the
-- session and sending the final reply to the client.
runUpdate :: Connection -> Phase Update -> App ()
runUpdate conn phase = finishSession phase >>= sendReply conn

-- | Handles the TRANSACTION state by attending and
-- responding user queries until the client quits.
runTrans :: Connection -> Phase Trans -> Reply -> App ()
runTrans conn phase reply = do
  sendReply conn reply

  query <- recvQuery conn

  case processQuery phase query of
    Stay phase' reply' -> runTrans  conn phase' reply'
    Next phase'        -> runUpdate conn phase'
    Term        reply' -> sendReply conn reply'
    Abrt               -> emit Warn "client disconnected"

-- | Tries to verify a client after identifying themselves.
-- On success advances to the TRANSACTION state,
-- else returns to the AUTHENTICATION state.
runVerif :: Connection -> Phase Verif -> App ()
runVerif conn auth = do
  result <- withAuth auth (runTrans conn)

  case result of 
    Left (phase, reply) -> runAuth conn phase reply
    Right ()            -> pure ()

-- | Handles the AUTHENTICATION state by attending
-- authentication user queries until the client
-- either finishes identifying or quits the session.
runAuth :: Connection -> Phase Auth -> Reply -> App ()
runAuth conn phase reply = do
  sendReply conn reply

  query <- recvQuery conn

  case processQuery phase query of
    Stay phase' reply' -> runAuth   conn phase' reply'
    Next phase'        -> runVerif  conn phase'
    Term        reply' -> sendReply conn reply'
    Abrt               -> emit Warn "client disconnected"

-- | Starts a POP3 session with an acquired client
-- by greeting them and running the AUTHENTICATION state.
runSession :: Connection -> App ()
runSession conn = 
  let (phase, reply) = startSession
  in do
    emit Info $ "new client: " <> show conn
    runAuth conn phase reply
    emit Info $ "bye client: " <> show conn


-- Listener Logic -------------------------------------------------------

-- | Continously accepts incoming connections on the given listener,
-- forking a new session thread for each one.
--
-- If an exception occurs accepting a connection the error is handled
-- by 'onAcceptErr' and the loop continues.
--
-- If an exeception occurs during a valid session, the error is handled
-- by 'onClientErr' and the session thread stops.
serveClients :: Listener -> App ()
serveClients listener = forever $
  forkClient listener runSession onClientErr `UIO.catch` onAcceptErr

-- | Acquires a listener and starts waiting for clients. 
--
-- If an exception occurs opening a listener, the error is handled
-- by 'onListenerErr' and the server stops.
startServer :: App ()
startServer = do
  emit Info "server ready"
  withListener serveClients `UIO.catch` onListenerErr


-- Main -----------------------------------------------------------------

-- | Builds the application environment and launches the server.
main :: IO ()
main = buildEnv >>= \case
  Left (ConfigErr err) -> emit Fail $ "failed to build config: " <> show err
  Left (ShadowErr err) -> emit Fail $ "failed to build shadow: " <> show err
  Right env            -> runApp startServer env
