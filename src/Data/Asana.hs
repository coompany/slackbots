{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Asana (
    Token(..),
    TokenType(..),
    Request(..),
    Empty,
    Project(..),
    Task(..),
    Workspace(..),
    Webhook(..),
    WebhookNew(..),
    WebhookNewRequest,
    Events(..), Event(..)
) where

import           Data.Aeson
import           GHC.Generics



data Project = Project {
    projectId   :: Int,
    projectName :: String
} deriving Show
instance FromJSON Project where
    parseJSON = withObject "project" $ \o ->
        Project <$> o .: "id" <*> o .: "name"


data Task = Task {
    taskId   :: Int,
    taskName :: String
} deriving Show
instance FromJSON Task where
    parseJSON = withObject "task" $ \o ->
        Task <$> o .: "id" <*> o.: "name"


data Workspace = Workspace {
    workspaceId   :: Int,
    workspaceName :: String
} deriving (Show)
instance FromJSON Workspace where
    parseJSON = withObject "workspace" $ \o ->
        Workspace <$> o .: "id" <*> o .: "name"


data WebhookResource = WebhookResource {
    resourceId   :: Int,
    resourceName :: String
} deriving (Show)
instance FromJSON WebhookResource where
    parseJSON = withObject "resource" $ \o ->
        WebhookResource <$> o .: "id" <*> o .: "name"

data Webhook = Webhook {
    webhookId       :: Int,
    webhookResource :: WebhookResource,
    webhookTarget   :: String
} deriving (Show)
instance FromJSON Webhook where
    parseJSON = withObject "webhook" $ \o ->
        Webhook <$> o .: "id" <*> o .: "resource" <*> o .: "target"

data WebhookNew = WebhookNew Int String deriving (Show)
instance ToJSON WebhookNew where
    toJSON (WebhookNew resource target) =
        object [ "resource" .= resource, "target" .= target ]


-- Empty type holds empty objects and is used for empty replies
data Empty = Empty deriving (Show)
instance FromJSON Empty where
    parseJSON = withObject "empty" $ \_ -> return Empty


-- Events objects posted by Asana to webhook targets
newtype Events = Events { events :: [Event] } deriving (Show, Generic)
instance FromJSON Events

data Event = Event {
    eventResource :: Int,
    eventUser     :: Int,
    eventType     :: String,
    eventAction   :: String
} deriving (Show)
instance FromJSON Event where
    parseJSON = withObject "event" $ \o ->
        Event <$> o .: "resource"
              <*> o .: "user"
              <*> o .: "type"
              <*> o .: "action"


-- Generic request definition (all requests have a single `data` property)
newtype Request a = Request a deriving (Show)
type WebhookNewRequest = Request WebhookNew
instance ToJSON a => ToJSON (Request a) where
    toJSON (Request r) = object [ "data" .= r ]


-- OAuth related types
data TokenType = Bearer deriving Show
data Token = Token (Maybe TokenType) String deriving Show
