import Data.List
import Data.Matrix

import Control.Monad
import Debug.Trace

-- some global constants
global_INITIAL_HEIGHT = 32
global_BLANK_CHAR = '_'
global_TRIANGLE_CHAR = '1'


--main
main = do
	line <- getLine

	let num_iter = read line
	let sierpinski = createSierpinski global_INITIAL_HEIGHT num_iter
	let pretty_str_result = unlines . Data.Matrix.toLists $ sierpinski

	putStr pretty_str_result



createSierpinski :: Int -> Int -> Matrix Char
createSierpinski height 0 = createSolidTriangle height -- base case.
createSierpinski 1 _ = createSolidTriangle 1 -- further iterations are useless bc we cannot draw them anymore
createSierpinski height n_iterations
	| height < 0  || n_iterations < 0 = error "error: height less than 0. OR Negative number of iterations."
	| otherwise = top_half <-> bot_half
		where
			next_height = height `div` 2

			next_sierpinski = createSierpinski next_height (n_iterations -1)
			blank_block = createBlank next_height next_height
			vertical_seperator = createBlank next_height 1

			top_half = blank_block <|> next_sierpinski <|> blank_block
			bot_half = next_sierpinski <|> vertical_seperator <|> next_sierpinski

-- structure of sierpinski triangle matrix.
-- +-----------------+
-- | BB1 | STT | BB1 |  <- top half
-- |-----------------|
-- | SST | BB2 | STT |  <- bot half
-- +-----------------+
-- SST := sub sierpinski triangle
-- BB1 := blank block. a block that contains the 'blank' character
-- BB2 := a 2nd blank block. this time a singl column vertical bar of 'blank' character. serves as a seperator between the bottom two SSTs

createBlank :: Int -> Int -> Matrix Char
createBlank n_rows n_cols 
	| n_rows < 0 || n_cols < 0 = error "error: row or column size less than 0"
	| otherwise = matrix n_rows n_cols (\(_,_) -> global_BLANK_CHAR)


createSolidTriangle :: Int -> Matrix Char
createSolidTriangle triangle_height
	| triangle_height < 0 = error "error: triangle height less than 0."
	| otherwise = left_tri <|> right_tri
		where
			triangle_width = (2*triangle_height) -1 
			
			--solid = matrix triangle_height triangle_height (\(_,_) -> global_TRIANGLE_CHAR)
			--left_tri = Data.Matrix.mapPos (\(r,c) elem -> if (r + c < triangle_height +1) then global_BLANK_CHAR else elem) solid
			left_tri = matrix triangle_height triangle_height (\(r,c) -> if (r + c < triangle_height +1) then global_BLANK_CHAR else global_TRIANGLE_CHAR)

			right_tri_temp = flipHorizontal left_tri -- flip the left triangle to get the right triangle.
			right_tri = if triangle_height > 1 then
				Data.Matrix.submatrix 1 triangle_height 2 triangle_height right_tri_temp -- remove 1st column. Otherwise triangle will have 2 center columns when we join the left/right together.
			else
				Data.Matrix.fromList 0 0 []


-- as a constraint from this problem on triangles. The width = (2*height) -1.
-- This is so that when you print the triangles they slopes look straight. Each next row has 2 additional characters (1 on each side) than the prev row.
-- triangles will be constructed from a left triangle and a right triangle. dashes are used to show 'blank' spots.

-- ---1 ---
-- --11 1--
-- -111 11-
-- 1111 111

flipHorizontal :: Matrix a -> Matrix a
flipHorizontal matrix = foldl f matrix [1..n_folds_to_perform]
	where
		n_rows = nrows matrix
		n_cols = ncols matrix

		n_folds_to_perform = (n_cols +1) `div` 2 

		f acc elem = Data.Matrix.switchCols elem (n_cols +1 -elem) acc 
