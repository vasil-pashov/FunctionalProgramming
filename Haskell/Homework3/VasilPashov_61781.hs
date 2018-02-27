--Task1
--lower left, upper right
type Rect  = (Int, Int, Int, Int)
type Point =  (Int, Int)

rectNodes :: Rect -> [Point]
rectNodes (lx, ly, ux, uy) = [(lx, ly), (ux, uy), (ux, ly), (lx, uy)]

between :: Int -> (Int, Int) -> Bool
between x (lower, upper) = x >= lower && x <= upper

inRect :: Point -> Rect -> Bool
inRect (x, y) (lx, ly, ux, uy) = x `between` (lx, ux) && y `between` (ly, uy)

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
getCol :: Int -> [[t]] -> [t]
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

delIdx :: Int -> [t] -> [t]
delIdx _ [] = []
delIdx 0 (x:xs) = xs
delIdx idx (x:xs) = x:(delIdx (idx + 1) xs)

allSaddles :: (Ord t, Eq t) => [[t]] -> [(Int, Int)]
allSaddles matrix = allSaddles' matrix 0
	where
		allSaddles' [] _ = []
		allSaddles' (x:xs) rowIdx = (rowSaddles (<=) rowMins) ++
									 (rowSaddles (>=) rowMaxes) ++
									  (allSaddles' xs (rowIdx + 1))
			where
				rowMaxes       = findOptimal (>=) x
				rowMins        = findOptimal (<=) x
				rowSaddles op rowOp = [(rowIdx, colIdx) | (colIdx, val) <- rowOp,
															let col = delIdx rowIdx (getCol colIdx matrix),
															all (`op` val) col]

hasSaddle :: (Ord t, Eq t) => [[t]] -> Bool
hasSaddle = not . null . allSaddles
--Task 4

asSumOfTwo :: Int -> [Int] -> Int
asSumOfTwo n lst = length [() | a <- lst, b <- lst, a + b == n]

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = null $ filter ((==0) . (n `mod`)) [x | x <- [2..(round $ sqrt $ fromIntegral n)]]

repr = repr' 1 []
	where
		repr' n primes
			| isPrime n = combs:(repr' next (n:primes))
			| otherwise = combs:(repr' next primes)
			where
				combs = asSumOfTwo n primes
				next = n + 1

-- Task 5
class Automaton t where
	match :: t -> String -> Bool

data DFA = DFA Int [Int] (Int -> Char -> Int)

instance Automaton DFA where
	match (DFA statsCnt finals delta) input = match' 0 input
		where
			match' currentState ""     = currentState `elem` finals
			match' (-1) _              = False
			match' currentState (x:xs) = match' (delta currentState x) xs

data NFA = NFA Int [Int] (Int -> Char -> [Int])

instance Automaton NFA where 
	match automaton@(NFA statesCnt finals delta) input = match' 0 input
		where
			match' currentState ""     = currentState `elem` finals
			match' (-1) _              = False
			match' currentState (x:xs) = any (`match'` xs) nextStates
				where nextStates = (delta currentState x)