f :: Int -> [Int] -> [Int] -- Complete this function
f _ [] = []
f n (elem:array) = take n (repeat elem) ++ f n array

-- This part handles the Input and Output and can be used as it is. Do not modify this part.
main :: IO ()
main = getContents >>=
       mapM_ print. (\(n:arr) -> f n arr). map read. words
