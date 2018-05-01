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


	-- calculate perimeter
	let perimeter = calculatePerimeter points

	-- print answer to STDOUT
	putStrLn . show $ perimeter


calculatePerimeter :: [(Int, Int)] -> Double
calculatePerimeter points = foldl (\acc elem -> acc + lengthOfSegment elem) 0 segments
	where
		segments = createSegmentsFromPoints points
		lengthOfSegment :: [(Int,Int)] -> Double
		lengthOfSegment s = sqrt . fromIntegral $ (  (fst (s!!1) - fst (s!!0))^2 + (snd (s!!1) - snd (s!!0))^2  ) -- distance equation. floored to an Int

createSegmentsFromPoints :: [(Int, Int)] -> [[(Int,Int)]]
createSegmentsFromPoints points 
	| length points <= 1 =  error "error" 
	| otherwise = zipWith (\a b -> [a,b]) points points'
	where
		points' = (tail points)++([head points]) -- move head to the end of list
		-- points' contains a left index shifted version of points.

