width :: Int
width = 8
height :: Int
height = 8
type Pos = (Int, Int)

type Board = [Pos]
glider::Board
glider = [(4, 2),(2 , 3), (4 , 3), (3 , 4), (4 , 4)]

seqn::[IO a] -> IO()
seqn [] = return()
seqn (a:as) = do a
                 seqn as
posAChar :: Board -> Pos -> Char
posAChar b (x,y) = if (elem (x,y) b) then 
                      if ( y > width) then '\n'
               	   	  else 
               	   		 'O'
                   else 
               	      if ( y > width) then '\n'
               	   	  else 
               	   		 '|'

showcells :: Board -> IO()
showcells b = putStr (map (posAChar b) [(x,y)| x <- [1..(width+1)], y <-[1..height] ] )

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1) , (x,y-1),
                          (x+1,y-1),(x-1,y),
                          (x+1,y),(x-1,y+1),
                          (x,y+1),(x+1,y+1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1 ,
              ((y-1) `mod` height) +1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs 

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

births b = [p | p <- rmdups (concat (map neighbs b)),
                isEmpty b p,
                liveneighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x:rmdups (filter ( /= x ) xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

wait :: Int -> IO()
wait n = seqn [return () | _ <- [1..n]]

cls :: IO()
cls = putStr "\n\n\n"

life :: Board -> IO()
life b = do cls
            showcells b
            wait 500000
            life (nextgen b)
