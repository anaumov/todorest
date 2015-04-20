{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Aeson (Value (Null), (.=), object)
import Data.Default (def)
import Data.Text.Lazy (Text)
import Network.HTTP.Types.Status (created201,
  internalServerError500, notFound404)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (Settings, defaultSettings,
  setFdCacheDuration, setPort)
import Network.Wai.Middleware.RequestLogger (logStdout,
  logStdoutDev)
import System.Environment (lookupEnv)
import Web.Scotty.Trans (ActionT, Options, ScottyT,
  defaultHandler, delete, get, json, jsonData, middleware,
  notFound, param, post, put, scottyOptsT, settings,
  showError, status, verbose)

import qualified Database.Persist as DB
import qualified Database.Persist.Postgresql as DB

import Database.Models (Task, TaskId, migrateAll)
import Config.AppConfig

main :: IO ()
main = getConfig >>= runApplication

runApplication :: Config -> IO ()
runApplication c = do
    o <- getOptions
    let r m = runReaderT (runConfigM m) c
    scottyOptsT o r r application


getOptions :: IO Options
getOptions = do
    let s = defaultSettings
        s' = setFdCacheDuration 0 s
    return def
        { settings = s'
        , verbose = 1
        }


newtype ConfigM a = ConfigM
    { runConfigM :: ReaderT Config IO a
    } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

type Error = Text


application :: ScottyT Error ConfigM ()
application = do
    runDB (DB.runMigration migrateAll)
    middleware logStdoutDev
    defaultHandler defaultH

    get "/tasks" getTasks
    post "/tasks" postTasks
    get "/tasks/:id" getTask
    put "/tasks/:id" putTask
    delete "/tasks/:id" deleteTask
    notFound notFoundA


type Action = ActionT Error ConfigM ()


defaultH :: Error -> Action
defaultH x = do
    status internalServerError500
    let o = object ["error" .= showError x]
    json o

getTasks :: Action
getTasks = do
    ts <- runDB (DB.selectList [] [])
    json (ts :: [DB.Entity Task])


postTasks :: Action
postTasks = do
    taskData <- jsonData
    runDB (DB.insert_ taskData)
    status created201
    json (taskData :: Task)

getTask :: Action
getTask = do
    taskId <- param "id"
    taskItem <- runDB (DB.get (toKey taskId))
    case taskItem of
      Nothing -> notFoundA
      Just t -> json (t :: Task)


putTask :: Action
putTask = do
    taskId <- param "id"
    taskData <- jsonData
    runDB (DB.repsert (toKey taskId) taskData)
    json (taskData :: Task)


deleteTask :: Action
deleteTask = do
    taskId <- param "id"
    runDB (DB.delete (toKey taskId :: TaskId))
    json Null

notFoundA :: Action
notFoundA = do
   status notFound404
   json Null

toKey :: DB.ToBackendKey DB.SqlBackend a => Integer -> DB.Key a
toKey i = DB.toSqlKey (fromIntegral (i :: Integer))

runDB :: (MonadTrans t, MonadIO (t ConfigM)) => DB.SqlPersistT IO a -> t ConfigM a
runDB q = do
  p <- lift (asks pool)
  liftIO (DB.runSqlPool q p)