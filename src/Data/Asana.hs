{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Asana where

import           Data.Aeson
import           Data.Function   (on)
import qualified Data.Text       as T (unpack)
import           Data.Time.Clock (UTCTime (..))
import           GHC.Generics
import qualified Utils



data Project = Project {
    projectId   :: Int,
    projectName :: String
} deriving Show
instance FromJSON Project where
    parseJSON = Utils.parseIdName Project


data Task = Task {
    taskId          :: Int,
    taskName        :: String,
    taskCompleted   :: Bool,
    taskCompletedAt :: Maybe UTCTime
} deriving Show
instance FromJSON Task where
    parseJSON = withObject "task" $ \o ->
        Task <$> o .: "id"
             <*> o .: "name"
             <*> o .: "completed"
             <*> o .:? "completed_at"


data Workspace = Workspace {
    workspaceId   :: Int,
    workspaceName :: String
} deriving (Show)
instance FromJSON Workspace where
    parseJSON = Utils.parseIdName Workspace


data WebhookResource = WebhookResource {
    resourceId   :: Int,
    resourceName :: String
} deriving (Show)
instance FromJSON WebhookResource where
    parseJSON = Utils.parseIdName WebhookResource

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


data User = User {
    userId   :: Int,
    userName :: String
} deriving (Show)
instance FromJSON User where
    parseJSON = Utils.parseIdName User


data Story = Story {
    storyId   :: Int,
    storyText :: String,
    storyBy   :: User,
    storyAt   :: UTCTime
} deriving (Show)
instance FromJSON Story where
    parseJSON = withObject "story" $ \o ->
        Story <$> o .: "id"
              <*> o .: "text"
              <*> o .: "created_by"
              <*> o .: "created_at"


-- Empty type holds empty objects and is used for empty replies
data Empty = Empty deriving (Show)
instance FromJSON Empty where
    parseJSON = withObject "empty" $ \_ -> return Empty


-- Events objects posted by Asana to webhook targets
newtype Events = Events { events :: [Event] } deriving (Show, Generic)
instance FromJSON Events

data EventType = TaskEvent | StoryEvent | ProjectEvent deriving (Show, Eq)
instance FromJSON EventType where
    parseJSON (String s) = case s of
        "task"    -> return TaskEvent
        "project" -> return ProjectEvent
        "story"   -> return StoryEvent
        _         -> fail ("unrecognized event type " ++ T.unpack s)

data ActionType = AddedAction | RemovedAction | ChangedAction
                | DeletedAction deriving (Show, Eq)
instance FromJSON ActionType where
    parseJSON (String s) = case s of
        "changed" -> return ChangedAction
        "added"   -> return AddedAction
        "removed" -> return RemovedAction
        "deleted" -> return DeletedAction
        _         -> fail ("unrecognized action type " ++ T.unpack s)

data Event = Event {
    eventResource :: Int,
    eventUser     :: Int,
    eventType     :: EventType,
    eventAction   :: ActionType
} deriving (Show, Eq)
instance FromJSON Event where
    parseJSON = withObject "event" $ \o ->
        Event <$> o .: "resource"
              <*> o .: "user"
              <*> o .: "type"
              <*> o .: "action"

-- This definition makes so that events are sorted on the resource id,
-- effectively filtering out events with the same resource id to build a Set.
instance Ord Event where
    compare = compare `on` eventResource


-- Generic request definition (all requests have a single `data` property)
newtype Request a = Request a deriving (Show)
type WebhookNewRequest = Request WebhookNew
instance ToJSON a => ToJSON (Request a) where
    toJSON (Request r) = object [ "data" .= r ]


-- OAuth related types
data TokenType = Bearer deriving Show
data Token = Token (Maybe TokenType) String deriving Show
