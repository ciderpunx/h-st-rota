{-# LANGUAGE OverloadedStrings #-}
module RotaReminder where

import RotaEvaluator
import RotaParser
import RotaPrivateData

import Data.List (sort)
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')
import System.IO

-- import qualified Data.ByteString as B
--
-- reads rota file, converting each line into a Command
-- with lineToCmd
-- returns a big list of commands for converting into tweets
rotaFileToCmds r = undefined

-- converts a line to a Command
lineToCmd c = undefined

-- make new rota, returns a list of tuples of user and job
makeRota = do 
    g<-newStdGen
    h<-newStdGen
    return $ zip (cycle $ shuffle' ps (length ps) h) (shuffle' js (length js) g)

makeTextRota = do 
    rs <- makeRota
    return . sort $ map (\x -> fst x ++ ": " ++ snd x) rs

writeRotaFile = do
    rs <- makeTextRota
    outh <- openFile rotaFile WriteMode
    mapM_ (hPutStrLn outh) rs
    hClose outh
    return ()
