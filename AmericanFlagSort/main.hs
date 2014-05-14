import qualified Data.Map as M
import qualified Data.Vector as V

-- TODO replace == with bit comparison
cond bucket item
	| item == bucket = 1
	| otherwise = 0

count arr radix = M.elems $ M.fromList [ (bucket, foldr (+) 0 $ map (cond bucket) arr) | bucket <- [0 .. radix - 1]]

-- TODO replace V.fromList
offset arr radix = M.elems $ M.fromList [ (i, foldr (+) 0 $ V.slice 0 i counts) | i <- [1 .. radix]]
	where counts = V.fromList $ count arr radix 
