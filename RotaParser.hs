{-# LANGUAGE OverloadedStrings #-}
module RotaParser where

import Control.Applicative ((*>))
import Data.Char
import Data.Attoparsec.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

data Tok    = U User 
            | S Status
            | J Job
            | E 
            | Err Bst
        deriving Show

data User   = User Bst
            | BadUser Bst
        deriving Show

data Status = TD
            | Finished
            | BadStatus Bst
        deriving Show

data Job     = Job Bst deriving Show

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
      then return $ U (User u)
      else return $ U (BadUser u)

parseStatus :: Parser Tok
parseStatus = do
    s <- string "#" *> takeTill (==' ')
    case C.map toLower s of 
      "todo" -> return $ S TD
      "done" -> return $ S Finished
      _       -> return $ S (BadStatus s)

parseJob :: Parser Tok
parseJob = do
    quote
    j <- takeTill quotey
    quote
    return $ J (Job j)

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
