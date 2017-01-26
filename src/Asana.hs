{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Asana (
    Reply(..),
    Project(..),
    Task(..),
    AsanaClient(..),
    Token(..),
    TokenType(..),
    clientEnv,
    mkClient
) where


import Servant.Client
import Servant.API
import Data.Aeson
import Data.Proxy (Proxy(..))
import Data.Text (pack)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)



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


-- Generic reply definition (all replies have a single `data` property)
data Reply a = Reply a deriving (Show)
type ProjectsReply = Reply [Project]
type TasksReply = Reply [Task]
instance FromJSON a => FromJSON (Reply a) where
    parseJSON = withObject "reply" $ \o -> do
        Reply <$> o .: "data"


-- API description
type AsanaAPI =
    "api" :> "1.0" :> Header "Authorization" Token :>
        "projects" :>
        (    Get '[JSON] ProjectsReply
        :<|> Capture "projectId" Int :> "tasks" :> Get '[JSON] TasksReply
        )

-- Client description
data AsanaClient = AsanaClient {
    projects :: ClientM ProjectsReply,
    projectTasks :: Int -> ClientM TasksReply
}

-- OAuth related types
data TokenType = Bearer deriving Show
data Token = Token (Maybe TokenType) String deriving Show
instance ToHttpApiData Token where
    toUrlPiece (Token Nothing tok) = pack tok
    toUrlPiece (Token (Just Bearer) tok) = pack $ "Bearer " ++ tok


api :: Proxy AsanaAPI
api = Proxy

-- Build a client given a token
mkClient :: Maybe Token -> AsanaClient
mkClient token = AsanaClient{..}
    where projects :<|> projectTasks = client api token

-- Get the right client environment to run the API client
clientEnv :: IO ClientEnv
clientEnv = do
    manager <- newManager tlsManagerSettings
    return (ClientEnv manager (BaseUrl Https "app.asana.com" 443 ""))
