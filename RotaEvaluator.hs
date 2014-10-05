{-# LANGUAGE OverloadedStrings #-}
module RotaEvaluator where

import RotaParser

import Data.Char 
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as B
import qualified Data.Text as T

-- We evaluate in 2 stages because sometimes having the AST available is
-- useful, eg in building feeds
eval :: Bst -> Bst
eval = toBst . toAST

toAST :: Bst -> Command
toAST str = 
    case ts of
      Right (SelfUser:User u:StatusTD:Job j:[])
        -> Command (User u) StatusTD (Job j)
      Right (SelfUser:User u:StatusDone:Job j:[])
        -> Command (User u) StatusDone (Job j)
      Right (SelfUser:_:BadStatus b:_)
        -> Err $ strCat ["Unknown status: ", b]
      Right (SelfUser:BadUser u:_:_)
        -> Err $ strCat ["Unknown user: ", u]
      Right (User u:_:_:_)
        -> Err $ strCat ["No mention of self, first user seen: ", u]
      Right (BadUser u:_:_:_)
        -> Err $ strCat ["No mention of self, in addition first user seen was unknown user: ", u]
      Right ts' 
        -> Err $ strCat ["Syntax error, from tokens: ", packStr $ show ts']
      Left fails  
        -> Err $ strCat ["Parse error, noparse",packStr fails]
  where
    ts = runP str

toBst :: Command -> Bst
toBst c = case c of
    Err e 
        -> e
    (Command u@(User _) status j@(Job _)) 
        -> case status of 
              StatusTD -> todo u j
              StatusDone -> finished u j
              _  -> error "Bad token for status in toBst"
    _ 
        -> error "Unimplemented Command in toBst"

todo, finished :: Tok -> Tok -> Bst
todo (User user) (Job job) = strCat ["@",user," don't forget to: '",job,"' #todo"]
todo _ _ = error "Unrecoginzed tokens in todo"


finished (User user) (Job job) = strCat ["@",user, " just finished doing: '", job, "' #done"]
finished _ _ = error "Unrecoginzed tokens in finished"

--------------------------------------------------------------------------------
-- Library functions, mostly for converting between Text, String and Bst types 
--------------------------------------------------------------------------------
strCat :: [Bst] -> Bst
strCat = foldr B.append ""

-- cf: http://stackoverflow.com/questions/3232074/what-is-the-best-way-to-convert-string-to-bytestring
packStr :: String -> Bst
packStr = encodeUtf8 . T.pack

-- cf: http://stackoverflow.com/questions/4702325/best-way-to-convert-between-char-and-word8
bsToStr :: Bst -> String
bsToStr = map (chr . fromEnum) . B.unpack
