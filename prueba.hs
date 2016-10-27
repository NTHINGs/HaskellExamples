guess :: String -> IO()
guess word = do putStr "> "
                xs <- getLine
                if xs == word then
             	   putStrLn "La tienes!"
                else
             	   do putStrLn (diff word xs)
             	      guess word