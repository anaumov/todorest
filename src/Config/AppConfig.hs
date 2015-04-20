{-# LANGUAGE OverloadedStrings #-}

module Config.AppConfig
    (Config(..)
    , getConfig
    )
where

import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Database.Persist as DB
import qualified Database.Persist.Postgresql as DB


data Config = Config
    { pool :: DB.ConnectionPool }

getConfig :: IO Config
getConfig = do
    p <- getPool
    return Config
        { pool = p }


getPool :: IO DB.ConnectionPool
getPool = do
    connString <- getConnectionString
    let dbPool = DB.createPostgresqlPool connString 1
        loogerType = runStdoutLoggingT
    loogerType dbPool

getConnectionString :: IO DB.ConnectionString
getConnectionString = do
    return $ createConnectionString
        [ ("host", "localhost")
        , ("port", "5432")
        , ("dbname", "todo_dev")
        ]

createConnectionString:: [(T.Text, T.Text)] -> DB.ConnectionString
createConnectionString rawConfig =
    let createConfBlock (key, value) = T.concat [key, "=", value]
    in  encodeUtf8 (T.unwords (map createConfBlock rawConfig))
