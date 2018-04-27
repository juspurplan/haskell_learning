import Text.Printf (printf)

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
-- solve l_bound r_bound a_coeffs b_exponents
--Complete this function--
solve l_bound r_bound as bs = area_result : volume_result : []
	where
		step_size = 0.001
		num_steps = floor ((fromIntegral . abs $ (r_bound - l_bound)) / step_size)

		-- xs := a list of all x points that we will evaluate at between l_bound and r_bound given the step_size
		-- evaluate our polynomial at all the x points in xs
		xs = (take num_steps) . (map (\a -> a+(fromIntegral l_bound))) . (map (\a -> a*step_size)) $ [0.0, 1.0 ..]
		ys = map (evaluatePolynomial as bs) xs

		area_result = sum . map (*step_size) $ ys
		volume_result = sum . map (\a -> step_size*pi*(a^2)) $ ys


evaluatePolynomial :: [Int] -> [Int] -> Double -> Double
-- evaluates the polynomial specified by the as and bs using the value x
evaluatePolynomial as bs x = sum terms
	where
		coeffs = map (fromIntegral) as
		exponents = map (fromIntegral) bs

		terms = map ($x) term_funcs

		calculate_term :: Double -> Double -> Double -> Double
		calculate_term a b x = a * (x**b)
		term_funcs = zipWith (\a b -> calculate_term a b) coeffs exponents


--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
