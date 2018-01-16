--Task 1
count :: Eq t => t -> [t] -> Int
count el = length . filter (==el)

remove :: Eq t => t -> [t] -> [t]
remove t [] = []
remove t (x:xs)
	| t == x    = remove t xs
	| otherwise =  x : remove t xs

remove' :: Eq t => t -> [t] -> [t]
remove' t = filter (/=t)

makeSet :: Eq t => [t] -> [t]
makeSet [] = []
makeSet (x:xs) = x : (makeSet $ remove' x xs)

histogram :: Eq t => [t] -> [(t, Int)]
histogram lst = [(e, count e lst) | e <- set]
	where set = makeSet lst

-- Task 2
repeaing :: Eq t => t -> [t] -> [t]
repeaing a lst = a:(takeWhile (==a) lst)

maxRepeated :: Eq t => [t] -> Int
maxRepeated lst = maxRepeated' lst 0
	where 
		maxRepeated' [] res = res
		maxRepeated' lst@(x:xs) res = maxRepeated' newLst newRes
			where
				repLen = length $ repeaing x xs
				newLst = drop repLen lst
				newRes = max res repLen
