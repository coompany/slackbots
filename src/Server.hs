{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Server (
    runServer
) where

import           Data.Asana               (Event (..), Events (..))

import           Control.Monad.IO.Class   (liftIO)
import           Data.List                (intercalate)
import           Network.Wai.Handler.Warp (run)
import           Servant



instance MimeUnrender OctetStream (Maybe Events) where
    mimeUnrender _ _ = Right Nothing

type XHookSecHeader = Header "X-Hook-Secret" String
type XHookSigHeader = Header "X-Hook-Signature" String

type ServerAPI =
    "webhooks" :> XHookSecHeader
               :> XHookSigHeader
               :> ReqBody '[JSON, OctetStream] (Maybe Events)
               :> Post '[JSON] (Headers '[XHookSecHeader] NoContent)

server :: Server ServerAPI
server = handleWebhook

handleWebhook :: Maybe String ->
                 Maybe String ->
                 Maybe Events ->
                 Handler (Headers '[XHookSecHeader] NoContent)
handleWebhook (Just secret) _ _ = return $ addHeader secret NoContent
handleWebhook _ (Just signature) (Just (Events events)) = do
    _ <- liftIO $ putStrLn $ show signature ++ "\n\n" ++ prettyEvents
    return $ addHeader "" NoContent
    where
        prettyEvent (Event res usr ty act) =
            "Resource:\t" ++ show res ++ "\n" ++
            "User:\t" ++ show usr ++ "\n" ++
            "Type:\t" ++ ty ++ "\n" ++
            "Action:\t" ++ act
        prettyEvents = intercalate "\n\n" $ map prettyEvent events



serverAPI :: Proxy ServerAPI
serverAPI = Proxy

app :: Application
app = serve serverAPI server

runServer :: Int -> IO ()
runServer port = run port app
