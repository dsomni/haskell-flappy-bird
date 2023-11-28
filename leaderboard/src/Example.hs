{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Example (runApp) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Object, Value (..), object, (.=))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Database.HDBC
import Database.HDBC.Sqlite3
import Network.Wai (Application)
import qualified Web.Scotty as S

-- Function to insert data into the database
insertData :: Connection -> Int -> String -> IO [[SqlValue]]
insertData conn intParam stringParam =
  quickQuery conn "INSERT INTO your_table_name (score, name) VALUES (?, ?)" [nToSql intParam, toSql stringParam]

getData :: Connection -> IO [[SqlValue]]
getData conn =
  quickQuery conn "SELECT * FROM your_table_name ORDER BY score DESC" []

-- Your database initialization logic
initializeDB :: IO Connection
initializeDB = connectSqlite3 "your_database_file.db"

createTable :: Connection -> IO Integer
createTable conn = do
  let query = "CREATE TABLE IF NOT EXISTS your_table_name (score INTEGER, name TEXT)"
  run conn query []

app' :: Connection -> S.ScottyM ()
app' conn = do
  S.get "/" $ do
    S.text "hello"

  -- Endpoint to handle storing data into the database
  S.get "/store-data" $ do
    score <- S.param "score" :: S.ActionM Int
    name <- S.param "name" :: S.ActionM String
    _ <- liftIO $ insertData conn score name
    S.text "Data stored successfully!"

  S.get "/results" $ do
    results <- liftIO $ getData conn
    S.json (convert results)

-- S.text (TL.decodeUtf8 $ TL.encodeUtf8 $ TL.pack $ show results)

runApp :: IO ()
runApp = do
  conn <- initializeDB
  createTable conn -- Call the createTable function during initialization
  S.scotty 8080 (app' conn)

convert :: [[SqlValue]] -> [Value]
convert = map converter
  where
    converter [(SqlInt64 score), (SqlByteString name)] = object ["name" .= (show name), "score" .= score]