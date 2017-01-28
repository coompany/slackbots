{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Asana (
    Token(..),
    TokenType(..),
    Request(..),
    Empty(..),
    Project(..),
    Task(..),
    Workspace(..),
    Webhook(..),
    WebhookNew(..),
    AsanaClient(..),
    clientEnv,
    mkClient
) where

import Data.Asana

import Servant.Client
import Servant.API
import Data.Proxy (Proxy(..))
import Data.Aeson (ToJSON(..), FromJSON(..), decode, encode, Value)
import Data.Aeson.Types (parseEither)
import qualified Data.Map.Strict as Map
import Data.Text (pack)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Media ((//))



-- Alternative JSON mime type that parses 'data'-encapsulated replies
data DataJSON
instance Accept DataJSON where
    contentType _ = "application" // "json"
instance (ToJSON a) => MimeRender DataJSON a where
    mimeRender _ = encode
instance (FromJSON a) => MimeUnrender DataJSON a where
    mimeUnrender _ input =
        case decode input :: Maybe (Map.Map String Value) of
            Just hm ->
                case Map.lookup "data" hm of
                    Just d -> parseEither parseJSON d
                    Nothing -> Left "Error parsing DataJSON 'data'"
            Nothing -> Left "Error parsing DataJSON"


-- Use OAuth token data-type as HTTP header
instance ToHttpApiData Token where
    toUrlPiece (Token Nothing tok) = pack tok
    toUrlPiece (Token (Just Bearer) tok) = pack $ "Bearer " ++ tok


-- API description
type AsanaAPI =
    "api" :> "1.0" :> Header "Authorization" Token :>
        (   "projects" :>
                (    Get '[DataJSON] [Project]
                :<|> Capture "projectId" Int :> "tasks" :> Get '[DataJSON] [Task]
                )
        :<|> "workspaces" :> Get '[DataJSON] [Workspace]
        :<|> "webhooks" :>
            (    QueryParam "workspace" Int :> Get '[DataJSON] [Webhook]
            :<|> ReqBody '[DataJSON] WebhookNewRequest :> Post '[DataJSON] Webhook
            :<|> Capture "webhookId" Int :> Delete '[DataJSON] Empty
            )
        )

-- Client description
data AsanaClient = AsanaClient {
    projects :: ClientM [Project],
    projectTasks :: Int -> ClientM [Task],
    workspaces :: ClientM [Workspace],
    webhooks :: Maybe Int -> ClientM [Webhook],
    newWebhook :: WebhookNewRequest -> ClientM Webhook,
    delWebhook :: Int -> ClientM Empty
}



api :: Proxy AsanaAPI
api = Proxy

-- Build a client given a token
mkClient :: Token -> AsanaClient
mkClient token = AsanaClient{..}
    where (projects :<|> projectTasks)
            :<|> workspaces
            :<|> (webhooks
                :<|> newWebhook
                :<|> delWebhook) = client api $ Just token

-- Get the right client environment to run the API client
clientEnv :: IO ClientEnv
clientEnv = do
    manager <- newManager tlsManagerSettings
    return (ClientEnv manager (BaseUrl Https "app.asana.com" 443 ""))
