--Contributed by Ron Watkins
module Main where

-- Enter your code here to complete this function
-- we start with fib(1) = 0. instead of fib(0) = 0
fib 1 = 0
fib 2 = 1
fib n = fib (n-1) + fib (n-2)


-- This part is related to the Input/Output and can be used as it is
-- Do not modify it
main = do
    input <- getLine
    print . fib . (read :: String -> Int) $ input
