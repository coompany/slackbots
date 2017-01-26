{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Server (
    runServer
) where


import Servant
import Network.Wai.Handler.Warp (run)



type ServerAPI =
    "webhooks" :>
        (       Header "X-Hook-Secret" String :> Post '[JSON] (Headers '[Header "X-Hook-Secret" String] ())
        :<|>    Post '[JSON] String
        )

server :: Server ServerAPI
server = handleHook :<|> handleWebhook

handleHook :: Maybe String -> Handler (Headers '[Header "X-Hook-Secret" String] ())
handleHook (Just secret) = return $ addHeader secret ()
handleHook Nothing = return $ addHeader "" ()

handleWebhook :: Handler String
-- TODO code me
handleWebhook = return "Hello"


serverAPI :: Proxy ServerAPI
serverAPI = Proxy

app :: Application
app = serve serverAPI server

runServer :: Int -> IO ()
runServer port = run port app
