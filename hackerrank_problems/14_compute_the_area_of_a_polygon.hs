import Data.List
import Control.Monad

main = do
	line <- getLine
	let num_points = read line :: Int

	-- get list of points. 
	points <- forM [0, 1 .. num_points-1] (\a -> do 
		line <- getLine
		let line_data = map (read :: String -> Int) $ (words line)
		let point@(x,y) = (line_data!!0, line_data!!1) -- extract x,y coordinates from line. package into tuple
		return point
		)


	-- calculate area
	let area = calculateArea points

	-- print answer to STDOUT
	putStrLn . show $ area


-- using "shoelace method" from wikipedia
calculateArea :: [(Int, Int)] -> Double
calculateArea points
	| length points <= 1 = error "error"
	| otherwise = 0.5 * fromIntegral (partialSum xs ys' - partialSum ys xs')
	where
		(xs, ys) = unzip points
		ys' = (tail ys) ++ [head ys]
		xs' = (tail xs) ++ [head xs]
		partialSum a b = sum $ zipWith (*) a b
