module Main where

import Control.Concurrent (forkIO)
import Control.Monad (void)

import Server
    ( Connection(connPeer),
      Listener,
      acquireClient,
      recvClient,
      sendClient,
      withClient',
      withListener,
      Connection,
      Listener )

import Query (buildQuery, Query, QueryErr)

import Log (emit, Severity(..))
import Session (processQuery, startSession, Phase, Auth, Transition (Stay, Next, Term), Authed, withAuth, Trans, finishSession, Reply, Update)
import Data.Foldable (for_)
import Serialize (Serialize(serialize))

sendReply :: Connection -> Reply -> IO ()
sendReply conn reply = serialize reply >>= sendClient conn

recvQuery :: Connection -> IO (Either QueryErr Query)
recvQuery conn = buildQuery <$> recvClient conn

runUpdate :: Connection -> Phase Update -> IO ()
runUpdate conn phase = do
  reply <- finishSession phase
  sendReply conn reply

runTrans :: Connection -> Phase Trans -> Reply -> IO ()
runTrans conn phase reply = do
  sendReply conn reply

  query <- recvQuery conn

  case processQuery phase query of
    Stay (curr, reply') -> runTrans  conn curr reply'
    Next next           -> runUpdate conn next
    Term reply'         -> mapM_ (sendReply conn) reply'

tryAuth :: Connection -> Phase Authed -> IO ()
tryAuth conn auth = do
  result <- withAuth auth (runTrans conn)
  for_ result (runAuth conn (fst startSession))

runAuth :: Connection -> Phase Auth -> Reply -> IO ()
runAuth conn phase reply = do
  sendReply conn reply

  query <- recvQuery conn

  case processQuery phase query of
    Stay (curr, reply') -> runAuth   conn curr reply'
    Next next           -> tryAuth   conn next
    Term reply'         -> mapM_ (sendReply conn) reply'

runSession :: Connection -> IO ()
runSession conn = do
  emit Info $ "new client: " ++ show (connPeer conn)
  uncurry (runAuth conn) startSession
  emit Info $ "bye client: " ++ show (connPeer conn)

serverLoop :: Listener -> IO ()
serverLoop listener = do
  conn <- acquireClient listener
  void . forkIO $ withClient' conn runSession
  serverLoop listener

main :: IO ()
main = withListener "pop3" serverLoop
