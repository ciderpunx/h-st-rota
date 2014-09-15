import RotaReminder

main = do 
    rs <- makeTextRota
    putStrLn "This is an automated cleaning rota reminder. Jobs are assigned randomly on the first of the month and a reminder sent out every week or so."
    putStrLn "I may have forgotten some important jobs, please tell me if so. Replies to this email ought to come through as expected\n"
    mapM_ putStrLn rs
