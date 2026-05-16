{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

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

onListenerErr :: SomeException -> App ()
onListenerErr err = emit Fail $ "failed to acquire listener: " <> show err

onAcceptErr :: SomeException -> App ()
onAcceptErr err = emit Fail $ "failed when accepting client" <> show err

onClientErr :: Connection -> SomeException -> App ()
onClientErr conn err = do
  sendReply conn excepReply
  emit Fail $ "failed while attending client" <> show err


-- Network Wrappers -----------------------------------------------------

sendReply :: Connection -> Reply -> App ()
sendReply conn reply = serialize reply >>= sendClient conn

recvQuery :: Connection -> App (Either QueryErr Query)
recvQuery conn = buildQuery <$> recvClient conn


-- Connection Logic -----------------------------------------------------

runUpdt :: Connection -> Phase Updt -> App ()
runUpdt conn phase = do
  reply <- finishSession phase
  sendReply conn reply

runTrns :: Connection -> Phase Trns -> Reply -> App ()
runTrns conn phase reply = do
  sendReply conn reply

  query <- recvQuery conn

  case processQuery phase query of
    Stay phase' reply' -> runTrns conn phase' reply'
    Next phase'        -> runUpdt conn phase'
    Term        reply' -> sendReply conn reply'
    Abrt               -> emit Warn "client disconnected"

runVerf :: Connection -> Phase Verf -> App ()
runVerf conn auth = do
  result <- withAuth auth (runTrns conn)

  case result of 
    Left (phase, reply) -> runAuth conn phase reply
    Right ()            -> pure ()

runAuth :: Connection -> Phase Auth -> Reply -> App ()
runAuth conn phase reply = do
  sendReply conn reply

  query <- recvQuery conn

  case processQuery phase query of
    Stay phase' reply' -> runAuth conn phase' reply'
    Next phase'        -> runVerf conn phase'
    Term        reply' -> sendReply conn reply'
    Abrt               -> emit Warn "client disconnected"

runSession :: Connection -> App ()
runSession conn = 
  let (phase, reply) = startSession
  in do
    emit Info $ "new client: " <> show conn
    runAuth conn phase reply
    emit Info $ "bye client: " <> show conn


-- Listener Logic -------------------------------------------------------

listenLoop :: Listener -> App ()
listenLoop listener = forever $
  forkClient listener runSession onClientErr `UIO.catch` onAcceptErr

startServer :: App ()
startServer = do
  emit Info "server ready"
  withListener listenLoop `UIO.catch` onListenerErr


-- Main -----------------------------------------------------------------

main :: IO ()
main = buildEnv >>= \case
  Left (ConfigErr err) -> emit Fail $ "failed to build config: " <> show err
  Left (ShadowErr err) -> emit Fail $ "failed to build shadow: " <> show err
  Right env            -> runApp startServer env
