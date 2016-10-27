listaverdaderos [] = True
listaverdaderos (x:xs) | x == False = False
                       | otherwise = listaverdaderos xs

concatenarlistadelistas [] = []
concatenarlistadelistas (x:xs) = [a | a <- x] ++ concatenarlistadelistas xs

producir 0 x = []
producir n x = [x] ++ producir (n - 1) x

seleccionar n xs = last (take n xs)

pertenece n [] = False
pertenece n (x:xs) | n == x = True
                   | otherwise = pertenece n xs


combinarordenadas :: Ord a => [a] -> [a] -> [a]
combinarordenadas xs [] = xs
combinarordenadas [] ys = ys
combinarordenadas (x:xs) (y:ys) | (x <= y) = x:(combinarordenadas xs (y:ys))
                                | otherwise = y:(combinarordenadas (x:xs) ys)
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = combinarordenadas (mergesort (prmitad xs)) (mergesort (segmitad xs))
     where
     	prmitad xs = take (length xs `div` 2) xs
        segmitad xs = drop (length xs `div` 2) xs