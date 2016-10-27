sumarlistas xs ys = zipWith (+) xs ys

promedio xs = (sum xs) `div` (length xs)

tupla n xs = [(n , x) | x <- xs]