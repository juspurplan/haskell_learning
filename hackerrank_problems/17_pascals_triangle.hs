import Data.List
import Control.Monad

main = do 
	line <- getLine
	let triangle_height = (read :: String -> Int) line

	-- calculate pascal triangle
	let triangle = createPascalTriangle triangle_height
	let str_triangle = map (map show) triangle
	putStr . unlines . (map unwords) $ str_triangle

createPascalTriangle :: Int -> [[Int]]
createPascalTriangle 1 = [[1]]
createPascalTriangle n 
	| n <= 0 = error "error"
	| otherwise = prev_triangle ++ [rowSuccessor prev_row]
		where
			prev_triangle = createPascalTriangle (n-1)
			prev_row = last prev_triangle

-- another method
-- successor function. rows -> rows
-- initial row is [1]
-- recursively create triangle using successor function
-- if possible, maybe do it with infinite lists? where triangle = some infinite list. and i can take 5 triangle.

rowSuccessor :: [Int] -> [Int]
rowSuccessor row = zipWith (+) row_padded row_padded'
	where
		row_padded = 0:row
		row_padded' = row ++ [0]

