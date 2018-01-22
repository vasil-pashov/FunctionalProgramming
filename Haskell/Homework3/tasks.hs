--Task1

--lower left, upper right
type Rect  = (Int, Int, Int, Int)
type Point =  (Int, Int)

rectNodes :: Rect -> [Point]
rectNodes (lx, ly, ux, uy) = [(lx, ly), (ux, uy), (ux, ly), (lx, uy)]

between :: Int -> Int -> Int -> Bool
between min max x = x >= min && x <= max

inRect :: Point -> Rect -> Bool
inRect (x, y) (lx, ly, ux, uy) = between lx ux x && between ly uy y

--r1 has node that is in r2
rectIntersect :: Rect -> Rect -> Bool
rectIntersect r1 r2 = any (`inRect` r2) (rectNodes r1)

rectArea :: Rect -> Int
rectArea (lx, ly, ux, uy) = (ux - lx) * (uy - ly)

countIntersects :: Rect -> [Rect] -> Int
countIntersects r rs = length (filter (`rectIntersect` r) rs) - 1

mostPopular :: [Rect] -> Rect
mostPopular rects = foldr1 morePopular rects
	where
		morePopular x y
			| xInter > yInter						= x 
			| (xInter == yInter) && (xArea > yArea) = x
			| otherwise								= y 
			where
				xInter = countIntersects x rects
				yInter = countIntersects y rects
				xArea = rectArea x
				yArea = rectArea y

-- Task 2
insertAt :: t -> Int -> [t] -> [t]
insertAt x idx lst = let (s,e) = splitAt idx lst in s ++ [x] ++ e

allInserts :: t -> [t] -> [[t]]
allInserts x lst = [insertAt x idx lst | idx <- [0..len]]
	where len = length lst

combinations :: [t] -> [[t]]
combinations [] = []
combinations lst = comb' lst [[]]
	where
		comb' [] res     = res
		comb' (x:xs) res = comb' xs (res ++ (concat $ map (x `allInserts`) res))

sameOnAll :: Eq t => [t] -> (t -> t) -> (t -> t) -> Bool
sameOnAll [] f g = True
sameOnAll (x:xs) f g 
	| (f x) == (g x) = sameOnAll xs f g
	| otherwise = False

check :: (Num t, Eq t) => [(t -> t)] -> [t] -> Int
check funs values = foldr maxFun 0 combs
	where
		combs = tail $ combinations funs
		maxFun funsComb res
			| hasMatch && funsLen > res = funsLen
			| otherwise 						  = res
			where
				composed = foldr1 (.) funsComb
				hasMatch = any (sameOnAll values composed) funs
				funsLen  = length funsComb

--Task 3
getCol ::Int -> [[t]] -> [t]
getCol idx m = map (!!idx) m

findOptimal :: (Eq t, Ord t) => (t -> t -> Bool) -> [t] -> [(Int, t)]
findOptimal _ [] = []
findOptimal op (x:xs) = findOptimal' xs 1 [(0, x)]
	where
		findOptimal' [] _ res = res
		findOptimal' (x:xs) idx res@(m:ms)
			| x `op` (snd m) = findOptimal' xs (idx + 1) [(idx, x)]
			| x == (snd m)   = findOptimal' xs (idx + 1) ((idx,x):res)
			| otherwise      = findOptimal' xs (idx + 1) res

transpose :: [[t]] -> [[t]]
transpose ([]:_) = []
transpose m = (map head m) : transpose (map tail m)

--hasSaddle :: (Ord t, Eq t) => [[t]] -> Bool
--hasSaddle m = hasSaddle' m or hasSaddle' (transpose m)
--	where hasSaddle' m = any rowHasSaddle m
--		where rowHasSaddle row = any maxIncol mins
--			where 
--				mins = findOptimal (<) row
--				maxIncol (idx, el) = el == (maximum $ getCol idx m)
