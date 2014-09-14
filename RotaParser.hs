{-# LANGUAGE OverloadedStrings #-}
module RotaParser where

import Control.Applicative ((*>))
import Data.Char
import Data.Attoparsec.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

data Tok    = User Bst
            | BadUser Bst
            | StatusTD
            | StatusDone
            | BadStatus Bst
            | Job Bst
            | E
            | Err Bst
        deriving (Show,Eq,Ord)

data Command = Command Tok Tok Tok deriving Show

type Bst    = B.ByteString
type Ignore = String

quotey :: Char -> Bool
quotey x = x =='"' || x =='\''

quote :: Parser Char
quote = satisfy quotey

validUsers :: [Bst]
validUsers = ["rotabott","ciderpunx","elnornor","pseyclipse"]

parseUser :: Parser Tok
parseUser = do 
     u <- string "@" *> takeTill (==' ')
     if any (==u) validUsers
      then return $ User u
      else return $ BadUser u

parseStatus :: Parser Tok
parseStatus = do
    s <- string "#" *> takeTill (==' ')
    case C.map toLower s of 
      "todo" -> return $ StatusTD
      "done" -> return $ StatusDone
      _       -> return $ BadStatus s

parseJob :: Parser Tok
parseJob = do
    quote
    j <- takeTill quotey
    quote
    return $ Job j

parseIgn :: Parser Tok
parseIgn = do 
  takeTill (\c -> c == '#' || c=='@' || quotey c )
  return E

parseTweet = do 
  parseIgn
  _ <- parseUser -- i.e. rotabott/me
  parseIgn
  them <- parseUser
  parseIgn
  status <- parseStatus
  parseIgn
  job <- parseJob
  parseIgn
  return $ Command them status job
