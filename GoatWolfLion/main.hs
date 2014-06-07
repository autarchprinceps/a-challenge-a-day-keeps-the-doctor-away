import Data.List

data Type = LW | LG | WG deriving(Show, Eq)
data Node = Node { t::Type, len::Int, next::[Node] } -- deriving(Show)

check 0 0 _ = []
check 0 _ 0 = []
check 0 _ _ = [LW]
check _ _ 0 = [LG]
check _ 0 _ = [WG]
check _ _ _ = [LW, LG, WG]

calcnew t goat lion wolf
	| t == LW = (goat+1, lion-1, wolf-1)
	| t == LG = (goat-1, lion-1, wolf+1)
	| t == WG = (goat-1, lion+1, wolf-1)

buildup i (goat, lion, wolf) typ = Node { t=typ, len=(i+1), next=(map calcandrek (check goat lion wolf)) }
	where calcandrek = buildup i $ calcnew typ goat lion wolf

search pathToHere subtree
	| null (next subtree) = [(len subtree, newPath)]  -- END
	| otherwise = concat $ map (search newPath) (next subtree)
	where newPath = pathToHere ++ [t subtree]

-- müssen im moment alle >= 1 sein
start goat lion wolf = (minimumBy (\a b -> compare (fst a) (fst b)) . concat) $ map (search []) $ map (\x -> buildup 0 (calcnew x goat lion wolf) x) [LW, LG, WG]
