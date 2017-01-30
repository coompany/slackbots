{-# LANGUAGE OverloadedStrings #-}

module Utils (
    tlsClientEnv,
    mkRequest,
    parseJSON2,
    parseJSON3,
    parseIdName
) where

import           Data.Aeson
import           Data.Aeson.Types       (Parser (..))
import qualified Data.Text              as T
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client


-- Get the right client environment to run the API client
tlsClientEnv :: String -> Int -> IO ClientEnv
tlsClientEnv host port = do
    manager <- newManager tlsManagerSettings
    return (ClientEnv manager (BaseUrl Https host port ""))


mkRequest :: ClientEnv -> ClientM a -> (a -> b) -> (ServantError -> b) -> IO b
mkRequest ce cm f g = either g f <$> runClientM cm ce


parseJSON2 :: (FromJSON a, FromJSON b) =>
                (a -> b -> c) -> T.Text -> T.Text ->
                (Value -> Parser c)
parseJSON2 f s1 s2 = withObject "o" $ \o -> f <$> o .: s1 <*> o .: s2
parseJSON3 :: (FromJSON a, FromJSON b, FromJSON c) =>
                (a -> b -> c -> d) -> T.Text -> T.Text -> T.Text ->
                (Value -> Parser d)
parseJSON3 f s1 s2 s3 = withObject "o" $ \o -> f <$> o .: s1 <*> o .: s2 <*> o .: s3

-- Common function to parse a datatype with "id" and "name".
parseIdName :: (FromJSON a, FromJSON b) => (a -> b -> c) -> Value -> Parser c
parseIdName f = parseJSON2 f "id" "name"
