{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Asana (
    Token(..),
    TokenType(..),
    Reply(..),
    Request(..),
    Project(..),
    Task(..),
    Workspace(..),
    Webhook(..),
    WebhookNew(..),
    AsanaClient(..),
    clientEnv,
    mkClient
) where

import Asana.Data

import Servant.Client
import Servant.API
import Data.Proxy (Proxy(..))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)



-- API description
type AsanaAPI =
    "api" :> "1.0" :> Header "Authorization" Token :>
        (   "projects" :>
                (    Get '[JSON] ProjectsReply
                :<|> Capture "projectId" Int :> "tasks" :> Get '[JSON] TasksReply
                )
        :<|> "workspaces" :> Get '[JSON] WorkspacesReply
        :<|> "webhooks" :>
            (    QueryParam "workspace" Int :> Get '[JSON] WebhooksReply
            :<|> ReqBody '[JSON] WebhookNewRequest :> Post '[JSON] WebhookReply
            :<|> Capture "webhookId" Int :> Delete '[JSON] DeleteReply
            )
        )

-- Client description
data AsanaClient = AsanaClient {
    projects :: ClientM ProjectsReply,
    projectTasks :: Int -> ClientM TasksReply,
    workspaces :: ClientM WorkspacesReply,
    webhooks :: Maybe Int -> ClientM WebhooksReply,
    newWebhook :: WebhookNewRequest -> ClientM WebhookReply,
    delWebhook :: Int -> ClientM DeleteReply
}


api :: Proxy AsanaAPI
api = Proxy

-- Build a client given a token
mkClient :: Maybe Token -> AsanaClient
mkClient token = AsanaClient{..}
    where (projects :<|> projectTasks)
            :<|> workspaces
            :<|> (webhooks
                :<|> newWebhook
                :<|> delWebhook) = client api token

-- Get the right client environment to run the API client
clientEnv :: IO ClientEnv
clientEnv = do
    manager <- newManager tlsManagerSettings
    return (ClientEnv manager (BaseUrl Https "app.asana.com" 443 ""))
