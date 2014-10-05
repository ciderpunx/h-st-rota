{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module RotaTweet where

import Network.HTTP.Types.Status
import Control.Exception.Base
import Web.Authenticate.OAuth
import Network.HTTP.Conduit
import Data.Aeson
import Data.Text (Text, pack, unpack)
import GHC.Generics
import System.IO

import RotaPrivateData
import RotaEvaluator
import RotaParser

data Tweet =
  Tweet { text :: !Text
        , created_at :: !Text
        , id :: !Integer
        , user :: TwitterUser
        } deriving (Show, Generic)

data TwitterUser = TwitterUser {  screen_name :: !Text } deriving (Show, Generic, Eq)

instance FromJSON Tweet
instance FromJSON TwitterUser
instance ToJSON Tweet
instance ToJSON TwitterUser

mentions :: Integer -> IO (Either String [Tweet]) 
mentions sinceId = do
  req <- parseUrl $ "https://api.twitter.com/1.1/statuses/mentions_timeline.json?contributor_details=1&since_id=" ++ show sinceId
  res <- withManager $ \m -> do
           -- OAuth Authentication. 'signOAuth' modifies the HTTP header adding the
           -- appropriate authentication.
           signedreq <- signOAuth myoauth mycred req
           httpLbs signedreq m
  return $ eitherDecode $ responseBody res

getLatestMentions :: IO [Tweet]
getLatestMentions = do 
  inh <- openFile "rota_lastrun" ReadMode
  sId <- hGetContents inh
  let sinceId = read sId::Integer
  ts <- mentions sinceId
  hClose inh
  case ts of
    Left _  -> return []
    Right ts'  -> if null ts'
                  then return []
                  else do outh <- openFile "rota_lastrun" WriteMode
                          hPrint outh (RotaTweet.id $ head ts')
                          hClose outh
                          return $ filter (\t -> screen_name (user t) /= pack "rotabott") ts'

getCmds :: IO [Command]
getCmds = do
    ts <- getLatestMentions
    mapM (return . toAST . packStr . unpack . text) ts 

sendTweet :: Bst -> IO (Either String Bst)
sendTweet x = do 
    req <- parseUrl "https://api.twitter.com/1.1/statuses/update.json" 
    let req' = req {checkStatus = checkStatus'}
        postReq = urlEncodedBody [("status", x)] req'
    res <- withManager $ \m -> do
             signedreq <- signOAuth myoauth mycred postReq
             httpLbs signedreq m
    case statusCode $ responseStatus res of
      200 -> return $ Right x
      _   -> return . Left $ "Twitter problems " ++ (read . show $ responseBody res) 
  where
    -- Here we override the default error handling -- for the 401 and 403 types we are likely to hit
    -- we handle them by returning a Left with the error message. There might be other 40X messages
    -- but they are genuinely unexpected so more like a real error. 403 gets returned on duplicates
    -- and 401 on unauthed. Twitter often locks accounts it thinks are spammy and it is possible
    -- we may try to create duplicates.
    checkStatus' s@(Status sci _) hs c =
       if sci >= 200 && sci <= 403
         then Nothing
         else Just $ toException $ StatusCodeException s hs c

makeTweetFromCmd :: Command -> String
makeTweetFromCmd = bsToStr . toBst
