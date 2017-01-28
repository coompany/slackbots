module Main where

import qualified Asana as Asana
import Server

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (forever)
import Data.List (intercalate)
import Servant
import Servant.Client
import System.Environment (getEnv)
import System.Exit (exitSuccess)



asanaClient :: IO Asana.AsanaClient
asanaClient = do
    t <- getEnv "ASANA_TOKEN"
    return $ Asana.mkClient (Asana.Token (Just Asana.Bearer) t)


makeRequest :: (Show a) => ClientM a -> (a -> String) -> IO String
makeRequest cm f = mapReply =<< runClientM cm =<< Asana.clientEnv
    where
        mapReply (Left err) = return $ "Error:\n" ++ show err
        mapReply (Right reply) = return (f reply)


handleInteraction :: String -> IO String
handleInteraction input = do
    client <- asanaClient
    case input of
        "0" -> exitSuccess
        "1" -> makeRequest (Asana.workspaces client) showWorkspaces
        "2" -> makeRequest (Asana.projects client) showProjects
        "3" -> do
            line <- getInput "Which project?"
            let request = Asana.projectTasks client (read line :: Int)
            makeRequest request showTasks
        "4" -> do
            line <- getInput "Which workspace?"
            let request = Asana.webhooks client $ Just (read line :: Int)
            makeRequest request show
        "5" -> do
            resource <- getInput "Which resource?"
            target <- getInput "Which target?"
            let webhook = Asana.WebhookNew (read resource :: Int) target
                request = Asana.newWebhook client $ Asana.Request webhook
            makeRequest request show
        "6" -> do
            webhookId <- getInput "Which webhook?"
            let request = Asana.delWebhook client (read webhookId :: Int)
            makeRequest request show
        otherwise -> return "Unrecognized option"
    where
        getInput q = putStrLn q >>= return getLine
        prettyPrintList f xs = intercalate "\n\n" $ map f xs

        showWorkspaces ws = prettyPrintList showWorkspace ws
        showWorkspace (Asana.Workspace id name) =
            "ID:\t" ++ show id ++ "\nName:\t" ++ name

        showProjects ps = prettyPrintList showProject ps
        showProject (Asana.Project id name) =
            "ID:\t" ++ show id ++ "\nName:\t" ++ name

        showTasks ts = prettyPrintList showTask ts
        showTask (Asana.Task id name) =
            "ID:\t" ++ show id ++ "\nName:\t" ++ name


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
