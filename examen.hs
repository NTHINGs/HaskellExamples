pos :: Eq a => a -> [a] -> Int
pos x []                 = 0
pos x (y:ys) | x == y    = pos x (drop (length ys) ys)
             | otherwise = 1 + pos x ys
