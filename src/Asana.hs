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
    Event(..),
    EventType(..),
    ActionType(..),
    AsanaClient(..),
    TasksClient(..),
    clientEnv,
    mkClient,
    mkRequest,
    runServer,
    WebhookHandler,
) where

import           Data.Asana
import qualified Utils

import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson
import           Data.Aeson.Types         (parseEither)
import           Data.List                (intercalate)
import qualified Data.Map.Strict          as Map
import           Data.Proxy               (Proxy (..))
import qualified Data.Text                as T (pack)
import           Network.HTTP.Media       ((//))
import           Network.Wai.Handler.Warp (run)
import           Servant
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
    toUrlPiece (Token Nothing tok)       = T.pack tok
    toUrlPiece (Token (Just Bearer) tok) = T.pack $ "Bearer " ++ tok


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
        :<|> "tasks" :> Capture "taskId" Int :>
            (    Get '[DataJSON] Task
            :<|> "stories" :> Get '[DataJSON] [Story]
            )
        :<|> "stories" :> Capture "storyId" Int :> Get '[DataJSON] Story
        )

-- Client description
data TasksClient = TasksClient {
    getTask     :: ClientM Task,
    taskStories :: ClientM [Story] }

data AsanaClient = AsanaClient {
    projects      :: ClientM [Project],
    projectTasks  :: Int -> ClientM [Task],
    workspaces    :: ClientM [Workspace],
    webhooks      :: Maybe Int -> ClientM [Webhook],
    newWebhook    :: WebhookNewRequest -> ClientM Webhook,
    delWebhook    :: Int -> ClientM Empty,
    mkTasksClient :: Int -> TasksClient,
    getStory      :: Int -> ClientM Story }



api :: Proxy AsanaAPI
api = Proxy

-- Build a client given a token
mkClient :: Token -> AsanaClient
mkClient token = AsanaClient{..}
    where
        (projects :<|> projectTasks)
            :<|> workspaces
            :<|> (webhooks
                :<|> newWebhook
                :<|> delWebhook)
            :<|> tasksClient
            :<|> getStory = client api $ Just token
        -- Build a TasksClient given a task id
        mkTasksClient tid = TasksClient{..}
            where
                getTask :<|> taskStories = tasksClient tid

-- Get the right client environment to run the API client
clientEnv :: IO ClientEnv
clientEnv = Utils.tlsClientEnv "app.asana.com" 443

mkRequest :: ClientM a -> (a -> b) -> (ServantError -> b) -> IO b
mkRequest cm f g = do
    ce <- clientEnv
    Utils.mkRequest ce cm f g


-- | The API to receive webhooks makes it so that the description in Servant is
-- somewhat counterintuitive. When you create a new webhook, Asana will POST to
-- the provided URL with an `X-Hook-Secret` header to which the server has to
-- reply with an empty body 200 and the same header. Later, webhook events are
-- sent to the same endpoint in JSON. We cannot describe two separate endpoints
-- in Servant because always the first will match, failing in one of the two
-- request cases. This is solved with a single endpoint that accepts both
-- requests without `Content-Type` (with `OctetStream`) and JSON requests.
-- We also need to specify an optional body with `Maybe`.

type XHookSecHeader = Header "X-Hook-Secret" String
type XHookSigHeader = Header "X-Hook-Signature" String

type ServerAPI =
    "webhooks" :> XHookSecHeader
               :> XHookSigHeader
               :> ReqBody '[JSON, OctetStream] (Maybe Events)
               :> Post '[JSON] (Headers '[XHookSecHeader] NoContent)

-- This dummy instance parses `Maybe Events` when `Content-Type` is
-- `OctetStream`. Always returns `Nothing`.
instance MimeUnrender OctetStream (Maybe Events) where
   mimeUnrender _ _ = Right Nothing


type WebhookHandler a = [Event] -> IO a

server :: WebhookHandler a -> Server ServerAPI
server = handleWebhook

handleWebhook :: WebhookHandler a
              -> Maybe String       -- XHookSecHeader
              -> Maybe String       -- XHookSigHeader
              -> Maybe Events       -- ReqBody
              -> Handler (Headers '[XHookSecHeader] NoContent)
-- Handle webhook creation POST with secret.
handleWebhook _ (Just secret) _ _ = return $ addHeader secret NoContent
-- Handle webhook events by piping them to the handler.
handleWebhook handler _ (Just signature) (Just (Events events)) = do
    liftIO $ handler events
    return $ addHeader "" NoContent


serverAPI :: Proxy ServerAPI
serverAPI = Proxy

app :: WebhookHandler a -> Application
app handler = serve serverAPI (server handler)

runServer :: Int -> WebhookHandler a -> IO ()
runServer port handler = run port (app handler)
