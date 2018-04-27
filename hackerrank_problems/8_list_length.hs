--Only fill up the blanks for the function named len
--Do not modify the structure of the template in any other way
len :: [a] -> Int
len list = foldl (\acc elem -> acc+1) 0 list
