import Control.Applicative
import Control.Monad
import System.IO
import Data.Function


main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    forM_ [1..n] $ \a0  -> do
        x_temp <- getLine
        let x = read x_temp :: Double

        -- my code
        let answer = f x 9
        putStrLn . show $ answer


getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret          


-- my code
f :: Double -> Int -> Double
-- f x order
f _ 0 = 1
f x order_n
    | order_n < 0 = error "error"
    | otherwise = nth_term + f x (order_n -1)
    where
        nth_term = (x^order_n)/(factorial order_n)
        factorial n = fromIntegral . product $ take n [1..]
