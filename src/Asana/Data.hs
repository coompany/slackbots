{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Asana.Data (
    Token(..),
    TokenType(..),
    Reply(..),
    Request(..),
    DeleteReply,
    Project(..),
    ProjectsReply,
    Task(..),
    TasksReply,
    Workspace(..),
    WorkspacesReply,
    Webhook(..),
    WebhooksReply,
    WebhookNew(..),
    WebhookNewRequest,
    WebhookReply
) where

import Data.Aeson
import Data.Text (pack)
import Servant.API (ToHttpApiData(..))



data Project = Project {
    projectId :: Int,
    projectName :: String
} deriving Show
instance FromJSON Project where
    parseJSON = withObject "project" $ \o -> do
        Project <$> o .: "id" <*> o .: "name"


data Task = Task {
    taskId :: Int,
    taskName :: String
} deriving Show
instance FromJSON Task where
    parseJSON = withObject "task" $ \o -> do
        Task <$> o .: "id" <*> o.: "name"


data Workspace = Workspace {
    workspaceId :: Int,
    workspaceName :: String
} deriving (Show)
instance FromJSON Workspace where
    parseJSON = withObject "workspace" $ \o -> do
        Workspace <$> o .: "id" <*> o .: "name"


data WebhookResource = WebhookResource {
    resourceId :: Int
} deriving (Show)
instance FromJSON WebhookResource where
    parseJSON = withObject "resource" $ \o -> do
        WebhookResource <$> o .: "id"

data Webhook = Webhook {
    webhookId :: Int,
    webhookResource :: WebhookResource,
    webhookTarget :: String
} deriving (Show)
instance FromJSON Webhook where
    parseJSON = withObject "webhook" $ \o -> do
        Webhook <$> o .: "id" <*> o .: "resource" <*> o .: "target"

data WebhookNew = WebhookNew Int String deriving (Show)
instance ToJSON WebhookNew where
    toJSON (WebhookNew resource target) =
        object [ "resource" .= resource, "target" .= target ]


-- Generic reply definition (all replies have a single `data` property)
data Reply a = Reply a deriving (Show)
-- Empty type holds empty objects and is used for empty replies
data Empty = Empty deriving (Show)
type DeleteReply = Reply Empty
type ProjectsReply = Reply [Project]
type TasksReply = Reply [Task]
type WorkspacesReply = Reply [Workspace]
type WebhooksReply = Reply [Webhook]
type WebhookReply = Reply Webhook
-- Replies are objects with the data property only
instance FromJSON a => FromJSON (Reply a) where
    parseJSON = withObject "reply" $ \o -> do
        Reply <$> o .: "data"
-- Empty data are empty objects
instance FromJSON Empty where
    parseJSON = withObject "empty" $ \o -> return Empty

-- Generic request definition (all requests have a single `data` property)
data Request a = Request a deriving (Show)
type WebhookNewRequest = Request WebhookNew
instance ToJSON a => ToJSON (Request a) where
    toJSON (Request r) = object [ "data" .= r ]


-- OAuth related types
data TokenType = Bearer deriving Show
data Token = Token (Maybe TokenType) String deriving Show
instance ToHttpApiData Token where
    toUrlPiece (Token Nothing tok) = pack tok
    toUrlPiece (Token (Just Bearer) tok) = pack $ "Bearer " ++ tok
