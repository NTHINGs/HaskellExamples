reverso :: [a] -> [a]
reverso [] = []
reverso (x:xs) = (reverso xs) ++ [x] 