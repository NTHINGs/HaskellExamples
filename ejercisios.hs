maxnum :: Int->Int->Int
maxnum x y = if x >= y then x else y

sumacuadrados :: Int -> Int
sumacuadrados n = sum (map (^ 2) [1..n])

nesimo :: Int->Int
nesimo n | n == 0 = 0
	  	 | n == 1 = 1
	  	 | otherwise = nesimo (n-1) + nesimo (n-2)

factormenor :: Int->Int
factormenor n = 