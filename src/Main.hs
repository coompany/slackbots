module Main where

import qualified Asana as Asana
import Server

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (forever)
import Servant
import Servant.Client
import System.Environment (getEnv)
import System.Exit (exitSuccess)



asanaClient :: IO Asana.AsanaClient
asanaClient = do
    t <- getEnv "ASANA_TOKEN"
    return $ Asana.mkClient (Asana.Token (Just Asana.Bearer) t)


makeRequest :: Show a => ClientM (Asana.Reply a) -> IO String
makeRequest cm = do
    res <- runClientM cm =<< Asana.clientEnv
    return $ case res of
        Left err -> "Error:\n" ++ show err
        Right (Asana.Reply reply) -> show reply


handleInteraction :: String -> IO String
handleInteraction input = do
    client <- asanaClient
    case input of
        "0" -> exitSuccess
        "1" -> makeRequest (Asana.workspaces client)
        "2" -> makeRequest (Asana.projects client)
        "3" -> do
            line <- getInput "Which project?"
            makeRequest (Asana.projectTasks client (read line :: Int))
        "4" -> do
            line <- getInput "Which workspace?"
            makeRequest (Asana.webhooks client $ Just (read line :: Int))
        "5" -> do
            resource <- getInput "Which resource?"
            target <- getInput "Which target?"
            let webhook = Asana.WebhookNew (read resource :: Int) target
            makeRequest (Asana.newWebhook client $ Asana.Request webhook)
        "6" -> do
            webhookId <- getInput "Which webhook?"
            makeRequest (Asana.delWebhook client (read webhookId :: Int))
        otherwise -> return "Unrecognized option"
    where
        getInput q = putStrLn q >>= return getLine


onTermination :: (Show e, Show a) => MVar Bool -> (Either e a -> IO ())
onTermination serverCom = onTermination'
    where
        onTermination' eit = do
            putMVar serverCom True
            case eit of
                Left e -> error $ "ERROR: " ++ show e
                Right a -> putStrLn $ "END: " ++ show a

waitForServer :: MVar Bool -> IO ()
waitForServer serverCom = do
    sc <- takeMVar serverCom
    if sc
        then putStrLn "Server finished..."
        else waitForServer serverCom


main :: IO ()
main = do
    serverCom <- handleServerStart
    forever interaction
    where
        handleServerStart = do
            sc <- newMVar False
            threadId <- forkFinally (runServer 8080) (onTermination sc)
            putStrLn $ "Server runnning in threadId " ++ show threadId
            return sc
        interaction = do
            putStrLn "\nAsana explorer:\n\
                     \1 - Print workspaces;\n\
                     \2 - Print projects;\n\
                     \3 - Print tasks;\n\
                     \4 - Print webhooks;\n\
                     \5 - New webhook;\n\
                     \6 - Delete webhook;\n\
                     \0 - Exit;\n"
            getLine >>= handleInteraction >>= putStrLn
