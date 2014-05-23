import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Bits as B

bucketify startBitToCheck numBitToCheck item = (B.shiftR (B.shiftL item startBitToCheck) ((B.bitSize a) - (startBitToCheck + numBitToCheck)))

cond startBitToCheck numBitToCheck bucket item
	| bucketify startBitToCheck numBitToCheck item == bucket = 1
	| otherwise = 0

count varr startBitToCheck numBitToCheck = V.fromList $ M.elems $ M.fromList [ (bucket, V.foldr (+) 0 $ V.map (cond startBitToCheck numBitToCheck bucket) arr) | bucket <- [0 .. (2 ^ numBitToCheck) - 1]] -- TODO 2 ^ X ?

offset varr startBitToCheck numBitToCheck = V.fromList $ M.elems $ M.fromList [ (i, V.foldr (+) 0 $ V.slice 0 i counts) | i <- [1 .. (2 ^ numBitToCheck)]]
	where counts = count arr startBitToCheck numBitToCheck 

swap varr idx1 idx2 = varr V.// [(idx1, varr V.! idx2), (idx2, varr V.! idx1)]

vinc vec idx = vec V.// [(idx, 1 + (vec V.! idx))]

afs varr startBitToCheck numBitToCheck subset = do
	current_idx = fst subset
	in_bucket = 0
-- TODO: mutable vectors ?
-- loop:
-- 	currbucket = bucketify $ varr ! current_idx
-- 	if currbucket == in_bucket then
--		finished = vinc finished, currbucket
--		current_idx++
--		if(in_bucket < (V.length offsets) - 1) then
--			if(current_idx > offsets ! (in_bucket + 1)) then
--				in_bucket++
--			end
--			continue
--		else
--			if(current_idx < (snd subset) - 1) then
--				continue
--			else
--				break
--			end
--		end
--	end
-- 	varr = swap varr, current_idx, (offsets ! currbucket) + (finished ! currbucket)
-- 	finished = vinc finished, currbucket
-- end
return varr V.// (foldr (++) [] (map (afs_changeset varr (startBitToCheck + numBitToCheck) numBitToCheck) ((filter (not . null) (map (\i -> if offsets V.! (i + 1) - offsets ! i > 1 then (offsets V.! i, offsets V.! (i + 1)) else []) [0 .. V.length offsets - 2])) ++ (V.last offsets, V.length varr))))

	where offsets = offset (V.slice (fst subset) (snd subset) varr)  startBitToCheck numBitToCheck
		  finished = V.replicate (V.length offsets) 0

afs_changeset varr startBitToCheck numBitToCheck subset = [(i, chvarr V.! i) | i <- [0 .. (V.length chvarr) - 1]]
	where chvarr = (V.slice (fst subset) (snd subset) (afs varr startBitToCheck numBitToCheck subset))

sort arr numBitToCheck = afs varr 0 numBitToCheck (0,(V.length varr))
