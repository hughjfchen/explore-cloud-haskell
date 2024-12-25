{-# LANGUAGE RecordWildCards #-}

module Logger
  ( chatMessageToStr,
    chatLogger,
    runChatLogger,
    logChatMessage,
    logStr,
  )
where

import Control.Distributed.Process
  ( Process,
    match,
    nsend,
    receiveWait,
    register,
  )
import Control.Distributed.Process.Node (LocalNode, forkProcess, runProcess)
-- import Control.Monad.IO.Class (liftIO)
import Types

chatMessageToStr :: ChatMessage -> Text
chatMessageToStr ChatMessage {..} =
  case from of
    Server -> message
    Client sender -> sender <> ": " <> message

chatLogger :: Process ()
chatLogger =
  receiveWait
    [ match $ \chatMessage -> do
        liftIO . putTextLn $ chatMessageToStr chatMessage
        chatLogger,
      match $ \str -> do
        liftIO . putStrLn $ str
        chatLogger
    ]

runChatLogger :: LocalNode -> IO ()
runChatLogger node = do
  logger <- forkProcess node chatLogger
  runProcess node $ register "chatLogger" logger

logChatMessage :: ChatMessage -> Process ()
logChatMessage = nsend "chatLogger"

logStr :: Text -> Process ()
logStr = nsend "chatLogger"
