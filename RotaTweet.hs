{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module RotaTweet where

import RotaPrivateData
import RotaEvaluator
import RotaParser

import Data.ByteString (ByteString)
import Web.Authenticate.OAuth
import Network.HTTP.Conduit
import Data.Aeson
import Data.Text (Text, pack, unpack)
import GHC.Generics
import System.IO

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
  req <- parseUrl $ "https://api.twitter.com/1.1/statuses/mentions_timeline.json?contributor_details=1&since_id=" ++ (show sinceId)
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
  let sinceId = (read sId)::Integer
  ts <- mentions sinceId
  hClose inh
  case ts of
    Left err  -> return []
    Right ts  -> if null ts
                  then return []
                  else do -- outh <- openFile "rota_lastrun" WriteMode
                          -- hPutStrLn outh (show . RotaTweet.id $ head ts)
                          -- hClose outh
                          return $ filter (\t -> (screen_name $ user t) /= pack "rotabott") ts 

getCmds :: IO [Command]
getCmds = do
    ts <- getLatestMentions
    ts' <- mapM (\t -> return . toAST . packStr . unpack $ text t) ts 
    return ts'
