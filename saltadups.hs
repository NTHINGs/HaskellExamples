saltaDups x [] = [x]
saltaDups x acc
          | x == head acc = acc
          | otherwise = x : acc

comprimir :: (Eq a) => [a] -> [a]
comprimir = foldr saltaDups[]


prueba 