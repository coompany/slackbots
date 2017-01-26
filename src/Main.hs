module Main where

import qualified Asana as Asana

import Servant
import Servant.Client
import System.Environment (getEnv)
import System.Exit (exitSuccess)



asanaClient :: IO Asana.AsanaClient
asanaClient = do
    t <- getEnv "ASANA_TOKEN"
    return $ Asana.mkClient (Just (Asana.Token (Just Asana.Bearer) t))

makeRequest :: Show a => ClientM (Asana.Reply a) -> IO String
makeRequest cm = do
    res <- runClientM cm =<< Asana.clientEnv
    return $ case res of
        Left err -> "Error:\n" ++ show err
        Right (Asana.Reply reply) -> show reply

handleInteraction :: String -> IO String
handleInteraction "0" = exitSuccess
handleInteraction "1" = do
    client <- asanaClient
    makeRequest (Asana.projects client)
handleInteraction "2" = do
    putStrLn "Which project?"
    line <- getLine
    client <- asanaClient
    makeRequest (Asana.projectTasks client (read line :: Int))
handleInteraction  _  = return "Unrecognized option"

main :: IO ()
main = do
    putStrLn "\nAsana explorer:\n\
             \1 - Print projects;\n\
             \2 - Print tasks;\n\
             \0 - Exit;\n"
    getLine >>= handleInteraction >>= putStrLn
    main
