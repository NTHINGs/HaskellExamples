penultimo xs = xs !! (length xs - 2)

longitud [] = 0
longitud (x:xs) = 1 + longitud xs

remover [] = []
remover (x:xs) | x `elem` xs = remover xs
               | otherwise = x : remover xs

duplicar [] = []
duplicar (x:xs) = [x] ++ [x] ++ duplicar xs

intercambiar (x,y) = (y,x)

rotar n xs = drop n xs ++ take n xs

rotardir a n xs | a == 0 = drop n xs ++ take n xs
                | a == 1 = drop (length xs - n) xs ++ reverse (drop n (reverse xs))

  