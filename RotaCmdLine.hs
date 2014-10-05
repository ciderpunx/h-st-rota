{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Time
import System.Console.GetOpt
import System.Environment
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

import RotaDB
import RotaTweet
import RotaParser
import RotaEvaluator
import RotaPrivateData

-- 1. Regenerate rota
--    * Make new Rota by permutation
--    * Write tasks to database
-- 2. Send reminder emails & tweets
--    * Look up rota tasks from database
--    * Compile a personalized email per user requiring a lookup of email from @username
--    * Send each email to the user concerned
--    * Generate a list of tweets to send
--    * Store them in the database 1 row per tweet               TODO: How to send them at intervals?
-- 3. Check for new tweets
--    * Get mentions, filter for ones not sent by self
--    * Parse into AST
--    * Add to queue in DB
-- 4. Send next tweet from queue
--    * Get next tweet from the list in the DB (i.e. oldest entry)
--    * Send and, if successful, remove it from the DB

data Options =
    Options { optMkRota :: Bool
            , optSendReminders :: Bool
            , optCheckMentions :: Bool
            , optSendNextTweet :: Bool
            } deriving Show

defaultOptions :: Options
defaultOptions =
    Options { optMkRota = False
            , optSendReminders = False
            , optCheckMentions = False
            , optSendNextTweet = False
            }

options :: [OptDescr (Options -> Options)]
options = 
    [ Option "m" ["mkRota"]        (NoArg (\ opts -> opts { optMkRota = True }))        "Regenerate rota, run monthly or as often as rota rotates"
    , Option "r" ["sendReminders"] (NoArg (\ opts -> opts { optSendReminders = True })) "Send reminder emails and tweets"
    , Option "c" ["checkMentions"] (NoArg (\ opts -> opts { optCheckMentions = True })) "Check for new tweets to parse" 
    , Option "n" ["sendNextTweet"] (NoArg (\ opts -> opts { optSendNextTweet = True })) "Send next tweet from tweet queue" 
    ]

myOpts :: [String] -> IO (Options, [String])
myOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip Prelude.id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where 
    header = "Usage: rotaDo [mrcn] "

makeRota :: IO ()
makeRota = do 
    g <- newStdGen
    h <- newStdGen
    let ts = zip (cycle $ shuffle' ps (length ps) h) (shuffle' js (length js) g)
    mapM_ addTaskAndTweet ts
    return ()
  where
    addTaskAndTweet t = do
      (startDay,endDay) <- thisMonthStartEnd
      addTask Todo {who=fst t,what=snd t,start=startDay,end=endDay} 
      addTweet $ makeTweetFromCmd (Command (User (packStr $ fst t)) StatusTD (Job (packStr $ snd t)))

thisMonthStartEnd :: IO (UTCTime,UTCTime)
thisMonthStartEnd =  do 
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
        (year, month, _) = toGregorian $ localDay zoneNow
        startDay = toTime (show year ++ "-" ++ show month ++ "-01 00:00:00")
        lastDOM  = gregorianMonthLength year month
        endDay   = toTime (show year ++ "-" ++ show month ++ "-" ++ show lastDOM ++ " 23:59:59")
    return (startDay,endDay)

checkTweets :: IO ()
checkTweets = do
  cs <- getCmds
  mapM_ (makeTweetFromCmd >> print) cs  -- Add each tweet to the tweet queue table in the database rather than printing
  return ()

main :: IO ()
main = 
    do (os, _) <- getArgs >>= myOpts 
       if optMkRota os 
       then makeRota
       else if optSendReminders os
       then putStrLn "send reminders"
       else if optCheckMentions os
       then checkTweets
       else if optSendNextTweet os
       then putStrLn "send next tweet"
       else error "No options specified [mrcn]"
