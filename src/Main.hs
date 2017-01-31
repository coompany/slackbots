module Main where

import qualified Asana
import qualified Slack

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad           (forever, when)
import           Data.Foldable           (traverse_)
import           Data.List               (intercalate)
import qualified Data.Set                as Set
import           Data.Time.Clock         (UTCTime (..), diffUTCTime,
                                          getCurrentTime)
import           Servant
import           Servant.Client
import           System.Environment      (getEnv)
import           System.Exit             (exitSuccess)



asanaClient :: IO Asana.AsanaClient
asanaClient = do
    t <- getEnv "ASANA_TOKEN"
    return $ Asana.mkClient (Asana.Token (Just Asana.Bearer) t)


handleInteraction :: Asana.AsanaClient -> String -> IO String
handleInteraction client input = case input of
        "0" -> exitSuccess
        "1" -> mkAsanaRequest (Asana.workspaces client) showWorkspaces
        "2" -> mkAsanaRequest (Asana.projects client) showProjects
        "3" -> do
            line <- getInput "Which project?"
            let request = Asana.projectTasks client (read line :: Int)
            mkAsanaRequest request showTasks
        "4" -> do
            line <- getInput "Which workspace?"
            let request = Asana.webhooks client $ Just (read line :: Int)
            mkAsanaRequest request show
        "5" -> do
            resource <- getInput "Which resource?"
            target <- getInput "Which target?"
            let webhook = Asana.WebhookNew (read resource :: Int) target
                request = Asana.newWebhook client $ Asana.Request webhook
            mkAsanaRequest request show
        "6" -> do
            webhookId <- getInput "Which webhook?"
            let request = Asana.delWebhook client (read webhookId :: Int)
            mkAsanaRequest request show
        _ -> return "Unrecognized option"
    where
        mkAsanaRequest cm f = Asana.mkRequest cm f (\e -> "Error:\n" ++ show e)
        getInput q = putStrLn q >>= return getLine
        prettyPrintList f xs = intercalate "\n\n" $ map f xs

        showWorkspaces = prettyPrintList showWorkspace
        showWorkspace (Asana.Workspace id name) =
            "ID:\t" ++ show id ++ "\nName:\t" ++ name

        showProjects = prettyPrintList showProject
        showProject (Asana.Project id name) =
            "ID:\t" ++ show id ++ "\nName:\t" ++ name

        showTasks = prettyPrintList showTask
        showTask (Asana.Task id name _ _) =
            "ID:\t" ++ show id ++ "\nName:\t" ++ name


onTermination :: (Show e, Show a) => MVar Bool -> Either e a -> IO ()
onTermination serverCom eit = do
    putMVar serverCom True
    case eit of
        Left e  -> error $ "ERROR: " ++ show e
        Right a -> putStrLn $ "END: " ++ show a

waitForServer :: MVar Bool -> IO ()
waitForServer serverCom = do
    sc <- takeMVar serverCom
    if sc
        then putStrLn "Server finished..."
        else waitForServer serverCom


webhookHandler :: Asana.AsanaClient -> Asana.WebhookHandler ()
webhookHandler client events = do
    let evtSet = Set.fromList events
    putStrLn $ prettyEvents evtSet
    mapM_ actOnEvent evtSet
    where
        prettyEvent (Asana.Event res usr ty act) =
            "Resource:\t" ++ show res ++ "\n" ++
            "User:\t" ++ show usr ++ "\n" ++
            "Type:\t" ++ show ty ++ "\n" ++
            "Action:\t" ++ show act
        prettyEvents = intercalate ("\n" ++ replicate 30 '-' ++ "\n") .
                                   map prettyEvent . Set.toList
        actOnEvent (Asana.Event res usr Asana.TaskEvent Asana.AddedAction) =
            putStrLn $ "New Task added! [" ++ show res ++ "]"
        actOnEvent (Asana.Event taskId usr Asana.TaskEvent Asana.ChangedAction) =
            let request = Asana.getTask (Asana.mkTasksClient client taskId)
            in traverse_ withTask =<< Asana.mkRequest request Just (const Nothing)
            where
                withTask (Asana.Task _ name _ (Just completedAt)) = do
                    currentTime <- getCurrentTime
                    when (diffUTCTime currentTime completedAt < 60) <$>
                        putStrLn $ "Task " ++ name ++ " completed!"
                withTask _ = return ()
        actOnEvent _ = return ()


main :: IO ()
main = do
    startSlack
    withClient <- asanaClient
    serverCom <- startWebhookServer (webhookHandler withClient)
    forever (interaction withClient)
    where
        startSlack = do
            token <- getEnv "SLACK_TOKEN"
            threadId <- forkIO $ Slack.startListeningDefault token
            putStrLn $ "Slack bot running in " ++ show threadId
        startWebhookServer handler = do
            sc <- newMVar False
            threadId <- forkFinally (Asana.runServer 8080 handler)
                                    (onTermination sc)
            putStrLn $ "Server runnning in " ++ show threadId
            return sc
        interaction client = do
            putStrLn "\nAsana explorer:\n\
                     \1 - Print workspaces;\n\
                     \2 - Print projects;\n\
                     \3 - Print tasks;\n\
                     \4 - Print webhooks;\n\
                     \5 - New webhook;\n\
                     \6 - Delete webhook;\n\
                     \0 - Exit;\n"
            getLine >>= handleInteraction client >>= putStrLn
