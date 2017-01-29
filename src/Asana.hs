{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

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
    mkClient,
    mkRequest
) where

import           Data.Asana
import qualified Utils

import           Data.Aeson         (FromJSON (..), ToJSON (..), Value, decode,
                                     encode)
import           Data.Aeson.Types   (parseEither)
import qualified Data.Map.Strict    as Map
import           Data.Proxy         (Proxy (..))
import           Data.Text          (pack)
import           Network.HTTP.Media ((//))
import           Servant.API
import           Servant.Client



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
                    Just d  -> parseEither parseJSON d
                    Nothing -> Left "Error parsing DataJSON 'data'"
            Nothing -> Left "Error parsing DataJSON"


-- Use OAuth token data-type as HTTP header
instance ToHttpApiData Token where
    toUrlPiece (Token Nothing tok)       = pack tok
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
    projects     :: ClientM [Project],
    projectTasks :: Int -> ClientM [Task],
    workspaces   :: ClientM [Workspace],
    webhooks     :: Maybe Int -> ClientM [Webhook],
    newWebhook   :: WebhookNewRequest -> ClientM Webhook,
    delWebhook   :: Int -> ClientM Empty
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
clientEnv = Utils.tlsClientEnv "app.asana.com" 443

mkRequest :: ClientM a -> (a -> b) -> (ServantError -> b) -> IO b
mkRequest cm f g = do
    ce <- clientEnv
    Utils.mkRequest ce cm f g
