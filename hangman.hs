
import System.IO

diff :: String -> String -> String
diff xs ys = [if elem x ys then x else '-' | x <- xs]
 
guess :: String -> IO()
guess word = do putStr "> "
                xs <- getLine
                if (length xs) <= (length word) then
                    if xs == word then
             	       putStrLn "La tienes!"
                    else
             	       do putStrLn (diff word xs)
             	          guess word
             	else
             		do
             		    putStrLn "Ingresa una palabra con menos letras!"
             		    guess word


getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c 

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
           	   do putChar x
           	      return []
              else
           	   do putChar '-'
           	      xs <- sgetLine
           	      return (x:xs)
           	
hangman :: IO()
hangman = do putStrLn "Piensa una palabra: "
             word <- sgetLine
             putStrLn "Intenta adivinarla: "
             guess word
