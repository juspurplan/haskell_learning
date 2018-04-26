import Data.List
import Data.Maybe

f :: [Int] -> [Int] -- Fill up this Function
f [] = []
f list = foldr (\index acc -> (list!!index):acc) [] valid_indices
	where 
		valid_indices = filter odd . take (length list) $ [0..]


-- This part deals with the Input and Output and can be used as it is. Do not modify it.
main = do
	inputdata <- getContents
	mapM_ (putStrLn. show). f. map read. lines $ inputdata
