import Data.List
import Control.Monad

-- main
main :: IO ()
main = do
	line <- getLine
	let num_test_cases = read line :: Integer
	--print num_test_cases {-debug line-}

	-- for each test case
	forM_ [0, 1 .. num_test_cases-1] (\a -> do
		line <- getLine
		let num_pairs = read line :: Integer
		--print num_pairs {-debug line-}

		-- create list of pairs (2-tuples)
		pairs <- forM [0, 1 .. num_pairs-1] (\a -> do
			line <- getLine
			let pair_data = map (read :: String->Integer) $ (words line)
			let pair@(x,y) = (pair_data!!0, pair_data!!1) -- extract x and y (0th and 1st "word" of each line) and package into a tuple
			return pair
			)

		-- perform isFunction on list of pairs. print result
		let result = isFunction pairs
		let result_string = case result of 
			True -> "YES"
			False -> "NO"
		putStrLn $ result_string
		return ()

		)


isFunction :: [(Integer, Integer)] -> Bool
isFunction pairs = not . hasDuplicates $ xs
	where
		(xs, ys) = unzip pairs
		hasDuplicates list = not ((length . nub $ list) == (length list))

