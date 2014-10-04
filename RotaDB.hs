{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module RotaDB where

import Data.Time
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Locale (defaultTimeLocale)

databaseFilePath :: String
databaseFilePath = "rota.db"

data Todo = Todo { who :: String
                 , what :: String
                 , start :: UTCTime 
                 , end :: UTCTime }
    deriving (Eq, Show)

initDatabase :: IO ()
initDatabase = do
    conn <- connectSqlite3 databaseFilePath
    _ <- run conn ("CREATE TABLE tasks" ++
                 " ( id    INTEGER PRIMARY KEY ASC " ++
                 " , who   TEXT NOT NULL " ++
                 " , what  TEXT NOT NULL " ++
                 " , start TEXT NOT NULL " ++
                 " , end   TEXT )") []
    _ <- run conn ("CREATE TABLE tweets" ++
                 " ( id    INTEGER PRIMARY KEY ASC " ++
                 " , text   TEXT NOT NULL )" ) []
    commit conn
    disconnect conn
    return ()

-- TODO this function does not disconnect from the DB. Should be an error. 
-- I think that the intermediate Todo objects will mean I can do something 
-- like I do in nextTweet. There should be a better way to do this.
currentTasksForUser :: String -> IO [[SqlValue]]
currentTasksForUser u = do
    -- TODO: check we got a valid user
    let sql = "SELECT who, what, start, end FROM tasks WHERE who = '" ++ u ++ "'"
    conn <- connectSqlite3 databaseFilePath
    quickQuery conn sql []
    -- TODO: construct a Todo per row with data from this query

-- FIFO queue. We don't care about the contents of tweets, just retrieving their text from the DB
nextTweet :: IO (Maybe String)
nextTweet = do
    let sql = "SELECT text FROM tweets ORDER BY id LIMIT 1"
    conn <- connectSqlite3 databaseFilePath
    rows <- quickQuery conn sql []
    if length rows == 1
    then do 
      let x = fromSql . head $ head rows
      disconnect conn
      return $ Just x
    else do 
      disconnect conn
      return Nothing

addTask :: Todo -> IO ()
addTask t = do
    conn <- connectSqlite3 databaseFilePath
    _ <- run conn "INSERT INTO tasks (id,who,what,start,end) VALUES (null, ?, ?, ?, ?)" 
            [toSql $ who t
             , toSql $ what t
             , toSql $ start t
             , toSql $ end t ]
    commit conn
    disconnect conn
    return ()

addTweet :: String -> IO ()
addTweet txt = do
    conn <- connectSqlite3 databaseFilePath
    _ <- run conn "INSERT INTO tweets (id,text) VALUES (null, ?)" [toSql txt] 
    commit conn
    disconnect conn
    return ()

testAddTask :: IO ()
testAddTask = addTask 
    Todo  { who ="Charlie" 
          , what = "Test this thing"
          , start = toTime "2014-01-01 00:00:00" 
          , end = toTime "2014-01-31 23:59:59" }

toTime :: String -> UTCTime
toTime x = readTime defaultTimeLocale "%F %X" x::UTCTime
