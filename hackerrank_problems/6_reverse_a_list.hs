rev :: [Int] -> [Int]
rev list = foldl (\acc elem -> elem:acc) [] list