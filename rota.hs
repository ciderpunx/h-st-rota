import Data.List (sort)
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

ps = [ "Charlie: "
     , "Nor: "
     , "Manishta: "
     ]

js = [ "hoovering the stairs" 
     , "cleaning the cooker"
     , "cleaning the fridge"
     , "hoovering hall/landing"
     , "cleaning/mopping kitchen"
     , "cleaning/hoovering front room"
     , "cleaning/mopping upstairs bathroom"
     , "cleaning/mopping downstairs bathroom"
     , "gardening"
     , "recycling, garden waste, bins and food waste"
     , "trimming hedge"
     , "cleaning kitchen sink"
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

