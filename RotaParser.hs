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
     u <- takeTill (==' ')
     _ <- char ' '
     if any (==u) validUsers
      then return $ User u
      else return $ BadUser u

parseStatus :: Parser Tok
parseStatus = do
    s <- takeTill (==' ')
    _ <- char ' '
    case C.map toLower s of 
      "todo"  -> return $ StatusTD
      "done"  -> return $ StatusDone
      _       -> return $ BadStatus s

parseJob :: Parser Tok
parseJob = do
    j <- takeTill quotey
    quote
    return $ Job j

parseIgn :: Parser Tok
parseIgn = do 
  takeTill (== ' ')
  return E

parseTok :: Parser Tok
parseTok = do
    tokId <- anyChar
    case tokId of
      '@'   -> parseUser
      '#'   -> parseStatus
      '\''  -> parseJob
      '"'   -> parseJob
      otherwise     -> parseIgn

parseTweet = do 
  tokens <- many1 parseTok
  return tokens
  -- return $ Command them status job
