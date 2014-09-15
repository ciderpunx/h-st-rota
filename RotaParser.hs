{-# LANGUAGE OverloadedStrings #-}
module RotaParser where

import Control.Applicative ((*>))
import Data.Char (isAlpha, toLower)
import Data.List (sort)
import Data.Attoparsec.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

data Tok    = SelfUser
            | User Bst
            | BadUser Bst
            | StatusTD
            | StatusDone
            | BadStatus Bst
            | Job Bst
            | E
        deriving (Show,Eq,Ord)

data Command = Command Tok Tok Tok 
             | Err Bst
        deriving Show

type Bst    = B.ByteString
type Ignore = String

quotey :: Char -> Bool
quotey x = x =='"' || x =='\''

quote :: Parser Char
quote = satisfy quotey

identifier = do 
    s <- takeWhile1 (\c -> isAlpha c || isDigit c || c=='_')
    skipSpace
    return s

validUsers :: [Bst]
validUsers = ["ciderpunx","elnornor","psyeclipse"]

selfUser :: Bst
selfUser = "rotabott"

adminUser :: Bst
adminUser = "ciderpunx"

parseUser :: Parser Tok
parseUser = do 
     u <- identifier
     let lcU = C.map toLower u
     if any (==lcU) validUsers
      then return $ User lcU
      else if (lcU==selfUser) 
      then return SelfUser
      else return $ BadUser lcU

parseStatus :: Parser Tok
parseStatus = do
    s <- identifier
    case C.map toLower s of 
      "todo"  -> return $ StatusTD
      "done"  -> return $ StatusDone
      _       -> return $ BadStatus s

parseJob :: Parser Tok
parseJob = do
    j <- takeTill quotey
    quote
    skipSpace
    return $ Job j

parseIgn :: Parser Tok
parseIgn = do 
  takeTill (== ' ')
  skipSpace
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

-- Note: We filter out empty tokens, and sort into order so that we ought to get
-- something like SelfUser, User, Status, Job for a valid command
parseTweet :: Parser [Tok]
parseTweet = do 
  tokens <- many1 parseTok
  return . filter (/= E) $ sort tokens

runP :: Bst -> Either String [Tok]
runP = parseOnly parseTweet
