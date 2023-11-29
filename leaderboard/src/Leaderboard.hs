{-# LANGUAGE OverloadedStrings #-}

module Leaderboard (runApp) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..), object, (.=))
import Data.Maybe (fromMaybe)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Database.HDBC
import Database.HDBC.PostgreSQL
import System.Environment (lookupEnv)
import Web.Scotty qualified as S
import qualified Data.ByteString.Char8 as C

-- Function to insert data into the database
insertData :: Connection -> Int -> String -> IO [[SqlValue]]
insertData conn intParam stringParam =
  quickQuery conn "INSERT INTO leaderboard (score, name) VALUES (?, ?) ON CONFLICT (name) DO UPDATE SET score = GREATEST(EXCLUDED.score, leaderboard.score)" [nToSql intParam, toSql stringParam]

getData :: Connection -> IO [[SqlValue]]
getData conn =
  quickQuery conn "SELECT * FROM leaderboard ORDER BY score DESC LIMIT 5" []

-- Your database initialization logic
initializeDB :: String -> IO Connection
initializeDB = connectPostgreSQL

createTable :: Connection -> IO Integer
createTable conn = do
  let query = "CREATE TABLE IF NOT EXISTS leaderboard (score INTEGER, name TEXT PRIMARY KEY)"
  run conn query []

app :: Connection -> S.ScottyM ()
app conn = do
  -- Endpoint to handle storing data into the database
  S.get "/store-data" $ do
    score <- S.param "score" :: S.ActionM Int
    name <- S.param "name" :: S.ActionM String
    _ <- liftIO $ insertData conn score name
    liftIO $ commit conn
    S.text "Data stored successfully!"

  S.get "/results" $ do
    results <- liftIO $ getData conn
    -- S.text (TL.decodeUtf8 $ TL.encodeUtf8 $ TL.pack $ show results)
    S.json (convert results)


runApp :: IO ()
runApp = do
  maybeConnectionString <- lookupEnv "DATABASE_CONNECTION_STRING"
  let connectionString = fromMaybe defaultConnectionString maybeConnectionString
  conn <- initializeDB connectionString
  _ <- createTable conn -- Call the createTable function during initialization
  S.scotty 8080 (app conn)
  where
    defaultConnectionString = "host=localhost dbname=postgres user=postgres password=postgres port=5432"

convert :: [[SqlValue]] -> [Value]
convert = map converter
  where
    converter [SqlInt64 score, SqlByteString name] = object ["name" .= C.unpack name, "score" .= score]
    converter [SqlInteger score, SqlByteString name] = object ["name" .= C.unpack name, "score" .= score]
    converter _ = object []