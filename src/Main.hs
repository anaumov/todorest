{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Default (def)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (Settings, defaultSettings,
  setFdCacheDuration, setPort)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Web.Scotty.Trans (Options, ScottyT, scottyOptsT, middleware, verbose, settings)

import qualified Database.Persist as DB
import qualified Database.Persist.Postgresql as DB

import Database.Models (migrateAll)
import Config.AppConfig (Config(..), getConfig)
import Routes(Error, ConfigM(..), defineRoutes, runDB)

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

application :: ScottyT Error ConfigM ()
application = do
    runDB (DB.runMigration migrateAll)
    middleware logStdoutDev
    defineRoutes