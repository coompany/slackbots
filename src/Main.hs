{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Asana                     as A
import qualified Slack                     as S

import           Control.Applicative       (empty)
import           Control.Concurrent        (forkFinally)
import           Control.Monad             (forever, when)
import           Control.Monad.Trans.Class (lift)
import           Data.Foldable             (traverse_)
import           Data.List                 (intercalate, isInfixOf)
import qualified Data.Set                  as Set
import           Data.Time.Clock           (UTCTime (..), diffUTCTime,
                                            getCurrentTime)
import           Servant
import           Servant.Client
import           System.Environment        (getEnv)
import           System.Exit               (exitSuccess)



handleInteraction :: A.AsanaClient -> String -> IO String
handleInteraction client input = case input of
        "0" -> exitSuccess
        "1" -> mkAsanaRequest (A.workspaces client) showWorkspaces
        "2" -> mkAsanaRequest (A.projects client) showProjects
        "3" -> do
            line <- getInput "Which project?"
            let request = A.projectTasks client (read line :: Int)
            mkAsanaRequest request showTasks
        "4" -> do
            line <- getInput "Which workspace?"
            let request = A.webhooks client $ Just (read line :: Int)
            mkAsanaRequest request show
        "5" -> do
            resource <- getInput "Which resource?"
            target <- getInput "Which target?"
            let webhook = A.WebhookNew (read resource :: Int) target
                request = A.newWebhook client $ A.Request webhook
            mkAsanaRequest request show
        "6" -> do
            webhookId <- getInput "Which webhook?"
            let request = A.delWebhook client (read webhookId :: Int)
            mkAsanaRequest request show
        _ -> return "Unrecognized option"
    where
        mkAsanaRequest cm f = A.mkRequest cm f (\e -> "Error:\n" ++ show e)
        getInput q = putStrLn q >>= return getLine
        prettyPrintList f xs = intercalate "\n\n" $ map f xs

        showWorkspaces = prettyPrintList showWorkspace
        showWorkspace (A.Workspace id name) =
            "ID:\t" ++ show id ++ "\nName:\t" ++ name

        showProjects = prettyPrintList showProject
        showProject (A.Project id name) =
            "ID:\t" ++ show id ++ "\nName:\t" ++ name

        showTasks = prettyPrintList showTask
        showTask (A.Task id name _ _) =
            "ID:\t" ++ show id ++ "\nName:\t" ++ name


webhookHandler :: A.AsanaClient -> A.WebhookHandler ()
webhookHandler client events = do
    let evtSet = Set.fromList events
    putStrLn $ prettyEvents evtSet
    mapM_ actOnEvent evtSet
    where
        prettyEvent (A.Event res usr ty act) =
            "Resource:\t" ++ show res ++ "\n" ++
            "User:\t" ++ show usr ++ "\n" ++
            "Type:\t" ++ show ty ++ "\n" ++
            "Action:\t" ++ show act
        prettyEvents = intercalate ("\n" ++ replicate 30 '-' ++ "\n") .
                                   map prettyEvent . Set.toList
        actOnEvent (A.Event res usr A.TaskEvent A.AddedAction) =
            putStrLn $ "New Task added! [" ++ show res ++ "]"
        actOnEvent (A.Event taskId usr A.TaskEvent A.ChangedAction) =
            let request = A.getTask (A.mkTasksClient client taskId)
            in traverse_ withTask =<< A.mkRequest request Just (const Nothing)
            where
                withTask (A.Task _ name _ (Just completedAt)) = do
                    currentTime <- getCurrentTime
                    when (diffUTCTime currentTime completedAt < 60) <$>
                        putStrLn $ "Task " ++ name ++ " completed!"
                withTask _ = return ()
        actOnEvent _ = return ()


rtmListener :: S.RTMListener
rtmListener self team users chans = \case
    (S.MessageEvt ch us tx ts Nothing) ->
        if S.selfId self `isInfixOf` tx
        then lift $ return (S.MessageReply 1 ch ("Hello <@" ++ us ++ ">"))
        else do
            lift $ putStrLn $ getChanName ch ++ ": " ++ getUserName us ++ " wrote " ++ tx
            empty
    (S.MessageEvt ch us tx ts (Just S.MeMessage)) -> do
        lift $ putStrLn $ getChanName ch ++ ": " ++ getUserName us ++ " is " ++ tx
        empty
    (S.MessageEvt ch us tx ts (Just S.ChannelJoin)) -> do
        lift $ putStrLn $ getChanName ch ++ ": " ++ getUserName us ++ " joined"
        empty
    (S.MessageEvt ch us tx ts (Just S.ChannelLeave)) -> do
        lift $ putStrLn $ getChanName ch ++ ": " ++ getUserName us ++ " left"
        empty
    (S.UserTypingEvt ch us) -> do
        lift $ putStrLn $ getChanName ch ++ ": " ++ getUserName us ++ " is typing"
        empty
    where
        lookupUsers = S.lookupIndex users
        lookupChans = S.lookupIndex chans
        getUserName = lookupUsers S.userName "An unknown user"
        getChanName = (:) '#' . lookupChans S.channelName "UNKNOWN"


main :: IO ()
main = do
    startSlack =<< getEnv "SLACK_TOKEN"
    withClient <-  A.mkClient `fmap` A.Token (Just A.Bearer)
                              `fmap` getEnv "ASANA_TOKEN"
    startWebhookServer 8080 (webhookHandler withClient)
    forever (interaction withClient)
    where
        crashedHandler s = putStrLn . (++) (s ++ " crashed: ") .show
        startSlack token = do
            threadId <- forkFinally (S.startListening token rtmListener) $
                    either (crashedHandler "Slack bot") return
            putStrLn $ "Slack bot running in " ++ show threadId
        startWebhookServer port handler = do
            threadId <- forkFinally (A.runServer port handler) $
                    either (crashedHandler "Asana server") return
            putStrLn $ "Server runnning in " ++ show threadId
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
