nim :: IO()
nim = do 
	    putStr "¿Cómo se llama el jugador 1?: "
	    jugador1 <- getLine
	    putStr "¿Cómo se llama el jugador 2?: "
	    jugador2 <- getLine
	    putStr "¿Cuantos asteríscos tendrá la fila más grande?: "
	    asteriscos <- getLine
	    generarasteriscos (read asteriscos :: Int)
	    lista <- generarlista (read asteriscos :: Int) 
	    turnouno jugador1 jugador2 lista

producir :: Int -> String -> String
producir 0 x = ""
producir n x = x ++ producir (n - 1) x

producirlista :: Int -> String -> [String]
producirlista 0 x = []
producirlista n x = [x] ++ producirlista (n - 1) x

generarasteriscos :: Int -> IO()
generarasteriscos 0 = return()
generarasteriscos asteriscos = do putStr (show asteriscos)
                                  putStr ":"
                                  putStr (show (producir asteriscos "*"))
                                  putStr "\n"
                                  jugar (asteriscos - 1)
generarlista :: Int -> [[String]]
generarlista 0 = []
generarlista asteriscos = producirlista asteriscos "*" ++ (generarlista asteriscos - 1)

turnouno :: String -> String -> [[String]] -> IO()
turnouno jugador1 jugador2 lista = do putStr "Jguador 1 "
                                      putStr jugador
                                      putStr "\n"
                                      putStr "¿De que fila deseas quitar asteriscos? (0 si terminas tu turno): "
                                      fila <- getLine
                                      if fila == "0" then 
                                         turnodos jugador2 lista
                                      else

turnodos jugador2 lista

                                               