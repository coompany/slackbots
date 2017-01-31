{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Slack (
    rtmStart,
    clientEnv,
    mkRequest,
    startListening,
    EventHandler, RTMListener,
    SelfInfo(..),
    ChannelInfo(..),
    UserInfo(..),
    TeamInfo(..),
    RtmStartReply(..),
    Event(..),
    MessageSubtype(..),
    lookupIndex
) where

import qualified Utils

import           Control.Exception  (throw)
import           Control.Monad      (forever)
import           Data.Aeson
import           Data.Foldable      (traverse_)
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (fromMaybe)
import           Data.Proxy         (Proxy (..))
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock    (UTCTime (..), secondsToDiffTime)
import           Data.Time.Format   (defaultTimeLocale, parseTimeM)
import           Network.URI        (URI (..), URIAuth (..), parseURI)
import qualified Network.WebSockets as WS
import           Servant.API
import           Servant.Client
import           Wuss               (runSecureClient)



data SelfInfo = SelfInfo
    { selfId   :: String
    , selfName :: String
    } deriving (Show)
instance FromJSON SelfInfo where
    parseJSON = Utils.parseIdName SelfInfo

data TeamInfo = TeamInfo
    { teamId   :: String
    , teamName :: String
    } deriving (Show)
instance FromJSON TeamInfo where
    parseJSON = Utils.parseIdName TeamInfo

data UserInfo = UserInfo
    { userId   :: String
    , userName :: String
    } deriving (Show)
instance FromJSON UserInfo where
    parseJSON = Utils.parseIdName UserInfo

data ChannelInfo = ChannelInfo
    { channelId   :: String
    , channelName :: String
    } deriving (Show)
instance FromJSON ChannelInfo where
    parseJSON = Utils.parseIdName ChannelInfo

-- Reply of the rtm.start Slack Web API endpoint
data RtmStartReply = RtmStartReply
    { rtmURL      :: String
    , rtmSelf     :: SelfInfo
    , rtmTeam     :: TeamInfo
    , rtmUsers    :: [UserInfo]
    , rtmChannels :: [ChannelInfo] }
instance FromJSON RtmStartReply where
    parseJSON (Object o) = RtmStartReply
        <$> o .: "url"
        <*> o .: "self"
        <*> o .: "team"
        <*> o .: "users"
        <*> o .: "channels"
instance Show RtmStartReply where
    show (RtmStartReply url self@(SelfInfo sId sName) team@(TeamInfo tId tName) users chs) =
        "RTM start reply\n\
        \Self: " ++ sId ++ " / " ++ sName ++ "\n\
        \URL: " ++ url ++ "\n\
        \Team: " ++ tId ++ " / " ++ tName ++ "\n\
        \Users: " ++ unwords (map userName users) ++ "\n\
        \Channels: " ++ unwords (map ((:) '#' . channelName) chs)

-- Encapsulates a timestamp, with logic to parse the weird Slack's format.
newtype EventTime = EventTime UTCTime deriving (Show)
instance FromJSON EventTime where
    parseJSON = withText "ts" (return .
                               EventTime .
                               fromMaybe defaultTs .
                               parseTimeM True defaultTimeLocale "%s" .
                               takeWhile (/= '.') .
                               T.unpack)
        where defaultTs = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)

-- Datatype to represent an event sent by the RTM API via WebSockets.
data Event
    = MessageEvt
        { msgEvtChannel :: String
        , msgEvtUser    :: String
        , msgEvtText    :: String
        , msgEvtTs      :: EventTime
        , msgSubtype    :: Maybe MessageSubtype }
    | UserTypingEvt
        { utEvtChannel :: String
        , utEvtUser    :: String }
    deriving (Show)

data MessageSubtype = MeMessage | ChannelJoin | ChannelLeave deriving (Show)
instance FromJSON MessageSubtype where
    parseJSON (String st) = case st of
        "me_message" -> return MeMessage
        "channel_join" -> return ChannelJoin
        "channel_leave" -> return ChannelLeave
        _ -> fail $ "unrecognized message subtype " ++ T.unpack st

instance FromJSON Event where
    parseJSON = withObject "event" $ \o ->
        o .: "type" >>= \case
            "message" -> MessageEvt
                <$> o .: "channel"
                <*> o .: "user"
                <*> o .: "text"
                <*> o .: "ts"
                <*> o .:? "subtype"
            "user_typing" -> UserTypingEvt
                <$> o .: "channel"
                <*> o .: "user"
            typ -> fail $ "unknown event type " ++ typ


-- SlackAPI servant definition. ATM it only includes rtm.start endpoint.
type SlackAPI =
    "api" :> QueryParam "token" String :> "rtm.start" :> Get '[JSON] RtmStartReply

api :: Proxy SlackAPI
api = Proxy

rtmStart :: Maybe String -> ClientM RtmStartReply
rtmStart = client api

clientEnv :: IO ClientEnv
clientEnv = Utils.tlsClientEnv "slack.com" 443

mkRequest :: ClientM a -> (a -> b) -> (ServantError -> b) -> IO b
mkRequest cm f g = do
    ce <- clientEnv
    Utils.mkRequest ce cm f g


-- Useful data structure to index a list of objects by their ids
type ResourceId = String
type ResourceMap a = Map.Map ResourceId a
-- Builds a `ResourceMap` from a `ResourceId` extractor and a list of elements.
buildResourceMap :: (a -> ResourceId) -> [a] -> ResourceMap a
buildResourceMap f = Map.fromList . map (\x -> (f x, x))
-- The following function applies the extractor to the found element, otherwise
-- returns the default value.
lookupIndex :: ResourceMap a -> (a -> b) -> b -> (ResourceId -> b)
lookupIndex rm f def rid = maybe def f (Map.lookup rid rm)

type UsersMap = ResourceMap UserInfo
buildUsersMap :: [UserInfo] -> UsersMap
buildUsersMap = buildResourceMap userId

type ChannelsMap = ResourceMap ChannelInfo
buildChannelsMap :: [ChannelInfo] -> ChannelsMap
buildChannelsMap = buildResourceMap channelId


-- Names for those types should be self descriptive.
type EventHandler a = Event -> IO a
type RTMListener a = SelfInfo -> TeamInfo -> UsersMap -> ChannelsMap -> EventHandler a

-- Initiate an RTM WebSockets channel. Accepts a token and an RTMListener.
startListening :: String -> RTMListener a -> IO ()
startListening token rtmListener =
    mkRequest (rtmStart (Just token)) Right Left >>= \case
        Left err -> throw err
        Right rtm@(RtmStartReply url self team users chs) ->
            case parseURI url of
                Just (URI _ (Just (URIAuth _ host _)) path _ _) ->
                    let usersMap = buildUsersMap users
                        channelsMap = buildChannelsMap chs
                        handler = rtmListener self team usersMap channelsMap
                    in runSecureClient host 443 path (wsApp handler)
                Nothing -> error "RTM reply URI not parseable"

-- Pipes parsed events received over a ws connection into an EventHandler.
wsApp :: EventHandler a -> WS.ClientApp ()
wsApp evtHandler conn = forever $ do
    msg <- WS.receiveData conn
    traverse_ evtHandler (decode msg :: Maybe Event)
