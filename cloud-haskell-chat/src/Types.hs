module Types
  ( ChatName,
    NickName,
    Host,
    ServerAddress,
    ClientPortMap,
    Sender (..),
    ChatMessage (..),
    JoinChatMessage (..),
  )
where

import Control.Distributed.Process (SendPort)
import Data.Binary

type ChatName = Text

type NickName = Text

type Host = Text

type ServerAddress = Text

type ClientPortMap = Map NickName (SendPort ChatMessage)

data Sender = Server | Client NickName
  deriving stock (Generic, Typeable, Eq, Show)

instance Binary Sender

data ChatMessage = ChatMessage
  { from :: Sender,
    message :: Text
  }
  deriving stock (Generic, Typeable, Show)

instance Binary ChatMessage

newtype JoinChatMessage = JoinChatMessage
  { clientName :: NickName
  }
  deriving stock (Generic, Typeable, Show)

instance Binary JoinChatMessage
