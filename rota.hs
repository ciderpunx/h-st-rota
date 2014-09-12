import Data.List (sort)
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

ps = [ "Charlie: "
     , "Nor: "
     , "Manishta: "
     ]

js = [ "Hoover or sweep stairs" 
     , "Clean cooker"
     , "Clean fridge"
     , "Hoover hall and landing"
     , "Clean and mop kitchen"
     , "Clean and hoover front room"
     , "Clean and mop upstairs bathroom"
     , "Clean and mop downstairs bathroom"
     , "Garden"
     , "Recycling, garden waste, bins and food waste"
     , "Trim hedge"
     , "Clean kitchen sink"
     ]

rota = do 
    g<-newStdGen
    h<-newStdGen
    return . sort $ zipWith (++) (cycle $ shuffle' ps (length ps) h) (shuffle' js (length js) g)

main = do 
    rs <- rota
    putStrLn "This is an automated cleaning rota reminder. Jobs are assigned randomly on the first of the month and a reminder sent out every week or so."
    putStrLn "I may have forgotten some important jobs, please tell me if so. Replies to this email ought to come through as expected\n"
    mapM_ putStrLn rs

