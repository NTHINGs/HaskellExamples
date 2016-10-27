k y ys = ys ++ [y]

reverso xs = foldr k [] xs

absoluto xs = map abs xs