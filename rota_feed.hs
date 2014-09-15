import RotaFeed
import RotaPrivateData

import Data.List
import System.IO

main :: IO ()
main = do 
  fec <- feedWithEntryCount
  if snd fec == 0
  then return ()
  else do outh <- openFile atomFile WriteMode
          hPutStr outh (fst fec)
          hClose outh
