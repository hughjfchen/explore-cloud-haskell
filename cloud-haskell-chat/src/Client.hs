module Client
  ( searchChatServer,
    launchChatClient,
  )
where

import Control.Concurrent (threadDelay)
import Control.Distributed.Process
  ( NodeId (..),
    Process,
    ProcessId,
    ReceivePort,
    WhereIsReply (..),
    expectTimeout,
    link,
    receiveChan,
    spawnLocal,
    whereisRemoteAsync,
  )
import Control.Distributed.Process.ManagedProcess.Client (callChan, cast)
import Control.Distributed.Process.Node
  ( initRemoteTable,
    newLocalNode,
    runProcess,
  )
import Logger (logChatMessage, logStr, runChatLogger)
import Network.Transport (EndPointAddress (..))
import Network.Transport.TCP (createTransport, defaultTCPAddr, defaultTCPParameters)
import Types

searchChatServer :: ChatName -> ServerAddress -> Process ProcessId
searchChatServer name serverAddr = do
  let addr = EndPointAddress $ encodeUtf8 serverAddr
      srvId = NodeId addr
  whereisRemoteAsync srvId $ toString name
  reply <- expectTimeout 1000
  case reply of
    Just (WhereIsReply _ (Just sid)) -> return sid
    _ -> searchChatServer name serverAddr

launchChatClient :: ServerAddress -> Host -> Int -> ChatName -> IO ()
launchChatClient serverAddr clientHost port name = do
  mt <- createTransport (defaultTCPAddr (toString clientHost) (show port)) defaultTCPParameters
  case mt of
    Left err -> print err
    Right transport -> do
      node <- newLocalNode transport initRemoteTable
      runChatLogger node
      runProcess node $ do
        serverPid <- searchChatServer name serverAddr
        link serverPid
        logStr "Joining chat server ... "
        logStr "Please, provide your nickname ... "
        nickName <- liftIO getLine
        rp <- callChan serverPid (JoinChatMessage nickName) :: Process (ReceivePort ChatMessage)
        logStr "You have joined the chat ... "
        void $ spawnLocal $ forever $ do
          msg <- receiveChan rp
          logChatMessage msg
        forever $ do
          chatInput <- liftIO getLine
          cast serverPid (ChatMessage (Client nickName) chatInput)
          liftIO $ threadDelay 500000
