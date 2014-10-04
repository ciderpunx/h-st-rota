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
    commit conn
    disconnect conn
    return ()

currentTasksForUser :: String -> IO [[SqlValue]]
currentTasksForUser user = do
    -- TODO: check we got a valid user
    let sql = "SELECT who, what, start, end FROM tasks WHERE who = '" ++ user ++ "'"
    putStrLn sql
    conn <- connectSqlite3 databaseFilePath
    rows <- quickQuery conn sql []
    disconnect conn
    -- TODO: construct a Todo per row with data from this query
    return rows

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

testAddTask :: IO ()
testAddTask = addTask 
    Todo  { who ="Charlie" 
          , what = "Test this thing"
          , start = toTime "2014-01-01 00:00:00" 
          , end = toTime "2014-01-31 23:59:59" }

toTime :: String -> UTCTime
toTime x = readTime defaultTimeLocale "%F %X" x::UTCTime
