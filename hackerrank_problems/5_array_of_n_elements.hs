fn :: Int -> [Int]
fn n
	| n < 0 = []
	| otherwise = take n [0,1..]