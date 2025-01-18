module Server
  ( serveChatRoom,
    broadcastMessage,
    messageHandler,
    joinChatHandler,
    disconnectHandler,
    launchChatServer,
  )
where

import Control.Concurrent (threadDelay)
import Control.Distributed.Process
  ( DiedReason (..),
    NodeId (..),
    PortMonitorNotification (..),
    Process,
    ProcessId (..),
    monitorPort,
    processNodeId,
    register,
    sendPortId,
    spawnLocal,
  )
import Control.Distributed.Process.Extras.Time (Delay (..))
import Control.Distributed.Process.ManagedProcess
  ( ActionHandler,
    CastHandler,
    ChannelHandler,
    InitResult (..),
    ProcessDefinition (..),
    UnhandledMessagePolicy (..),
    defaultProcess,
    handleCast,
    handleInfo,
    handleRpcChan,
    serve,
  )
import Control.Distributed.Process.ManagedProcess.Server (continue, replyChan)
import Control.Distributed.Process.Node
  ( initRemoteTable,
    newLocalNode,
    runProcess,
  )
import qualified Data.Map as M (delete, elemAt, empty, filter, insert, member)
import Logger (logStr, runChatLogger)
import Network.Transport.TCP (createTransport, defaultTCPAddr, defaultTCPParameters)
import Types

serveChatRoom :: Host -> Int -> ChatName -> IO ()
serveChatRoom host port name = do
  mt <- flip createTransport defaultTCPParameters $ defaultTCPAddr (toString host) (show port)
  case mt of
    Right transport -> do
      node <- newLocalNode transport initRemoteTable
      runChatLogger node
      runProcess node $ do
        pId <- launchChatServer
        logStr $ "Server launched at: " <> show (nodeAddress . processNodeId $ pId)
        register (toString name) pId
        liftIO $ forever $ threadDelay 500000
    Left err -> print err

broadcastMessage :: ClientPortMap -> ChatMessage -> Process ()
broadcastMessage clientPorts msg =
  forM_ clientPorts (`replyChan` msg)

messageHandler :: CastHandler ClientPortMap ChatMessage
messageHandler = handler
  where
    handler :: ActionHandler ClientPortMap ChatMessage
    handler clients msg = do
      broadcastMessage clients msg
      continue clients

joinChatHandler :: ChannelHandler ClientPortMap JoinChatMessage ChatMessage
joinChatHandler sendPort = handler
  where
    handler :: ActionHandler ClientPortMap JoinChatMessage
    handler clients JoinChatMessage {..} =
      if clientName `M.member` clients
        then replyChan sendPort (ChatMessage Server "Nickname already in use ... ") >> continue clients
        else do
          void $ monitorPort sendPort
          let clients' = M.insert clientName sendPort clients
              msg = clientName <> " has joined the chat ..."
          logStr msg
          broadcastMessage clients $ ChatMessage Server msg
          continue clients'

disconnectHandler :: ActionHandler ClientPortMap PortMonitorNotification
disconnectHandler clients (PortMonitorNotification _ spId reason) = do
  let search = M.filter (\v -> sendPortId v == spId) clients
  case (null search, reason) of
    (False, DiedDisconnect) -> do
      let (clientName, _) = M.elemAt 0 search
          clients' = M.delete clientName clients
      broadcastMessage clients' (ChatMessage Server $ clientName <> " has left the chat ... ")
      continue clients'
    _ -> continue clients

launchChatServer :: Process ProcessId
launchChatServer =
  let server =
        defaultProcess
          { apiHandlers =
              [ handleRpcChan joinChatHandler,
                handleCast messageHandler
              ],
            infoHandlers = [handleInfo disconnectHandler],
            unhandledMessagePolicy = Log
          }
   in spawnLocal $ serve () (const (return $ InitOk M.empty Infinity)) server
