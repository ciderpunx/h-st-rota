{-# LANGUAGE OverloadedStrings #-}
module RotaEvaluator where

import RotaParser
import RotaTweet

import Data.Attoparsec.Char8
import Data.Char 
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as B
import qualified Data.Text as T

cliEval xs = 
  case runP xs of
    Right cmd -> eval cmd
    Left fail -> "Couldn't parse tweet"
    
runP = parseOnly parseTweet 

tweetStrings = do 
    ts <- getLatestMentions
    ts' <- mapM (\t -> return . cliEval . packStr . T.unpack $ text t) ts 
    return ts'

-- used in RotaFeed to make entries
getCmds = do
    ts <- getLatestMentions
    ts' <- mapM (\t -> return . getAST . packStr . T.unpack $ text t) ts 
    return ts'

getAST xs = 
    case runP xs of
      Right cmd -> cmd
      Left fail -> Command (U (User "rotabott")) (S (BadStatus "Parse Error")) (Err (packStr fail))

eval (Command user status job) = 
  case status of 
    S TD -> todo user job
    S Finished -> finished user job
    S (BadStatus s) -> error (bsToStr s) -- user job

strCat :: [Bst] -> Bst
strCat = foldr B.append ""

todo, finished :: Tok -> Tok -> Bst
todo (U (User user)) (J (Job job)) = strCat ["@",user," don't forget to: '",job,"' #todo"]

finished (U (User user)) (J (Job job)) = strCat ["@",user, " just finished doing: '", job, "' #done"]

-- cf: http://stackoverflow.com/questions/3232074/what-is-the-best-way-to-convert-string-to-bytestring
packStr :: String -> Bst
packStr = encodeUtf8 . T.pack

-- cf: http://stackoverflow.com/questions/4702325/best-way-to-convert-between-char-and-word8
bsToStr :: Bst -> String
bsToStr = map (chr . fromEnum) . B.unpack
