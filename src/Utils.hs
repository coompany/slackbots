module Utils (
    tlsClientEnv,
    mkRequest
) where

import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client


-- Get the right client environment to run the API client
tlsClientEnv :: String -> Int -> IO ClientEnv
tlsClientEnv host port = do
    manager <- newManager tlsManagerSettings
    return (ClientEnv manager (BaseUrl Https host port ""))


mkRequest :: ClientEnv -> ClientM a -> (a -> b) -> (ServantError -> b) -> IO b
mkRequest ce cm f g = mapReply =<< runClientM cm ce
    where
        mapReply (Left err)    = return (g err)
        mapReply (Right reply) = return (f reply)
