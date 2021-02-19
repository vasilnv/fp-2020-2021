import Data.Char

main = do print True
          print 2
          print "hahaha"
          print 3.2
          print [3,4,3]
          putStrLn "hello, world"
          putStrLn "Tell me your name:"
          name <- getLine
          let bigName = map toUpper name
          putStrLn ("Hey " ++ bigName ++ ", you rock")


putStr' :: String -> IO ()  
putStr' [] = return ()  
putStr' (x:xs) = do  
    putChar x  
    putStr' xs  