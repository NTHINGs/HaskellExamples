letra :: IO()
letra = do putStr "Ingresa una letra que deseas hacer grande(EN MAYUSCULA)\n"
           x <- getLine
           putStrLn "----------------------------------------------------"
           putStr (buscarLetra x)

buscarLetra :: String -> String
buscarLetra x | x == "A" = "  AAA  \n\ 
                           \ AAAAA \n\ 
                           \AA   AA \n\ 
                           \AAAAAAA \n\ 
                           \AA   AA \n"
              | x == "B" = "BBBBB\n\   
                           \BB   B\n\  
                           \BBBBBB\n\  
                           \BB   BB\n\ 
                           \BBBBBB\n"
              | otherwise = ""