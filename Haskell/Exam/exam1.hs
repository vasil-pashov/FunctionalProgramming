{-# LANGUAGE DeriveFunctor#-}

import Data.List

-- Task 1

approximate f l = resFn
	where resFn x = fst $ minimumBy cmp [(g x,  abs (f x - g x)) | g <- l]

cmp (fn1, d1) (fn2, d2)
	| d1 > d2 = GT
	| d1 == d2 = EQ
	| d1 < d2 = LT

-- Task 2

iterator lst@(x:xs) f = all (\(a, b) -> f a == b) pairs
	where pairs = zip lst xs

-- Task 3
t = Node 5 (Node 3 (Node 1 Empty (Node 2 Empty Empty)) (Node 4 Empty Empty)  ) (Node 6 (Node 8 (Node 9 Empty Empty) (Node 7 Empty Empty)) Empty)


data Tree t = Empty | Node t (Tree t) (Tree t)

onLvl 0 Empty = []
onLvl 0 (Node root _ _) = [root]
onLvl lvl (Node _ Empty Empty) = []
onLvl lvl (Node _ Empty r) = onLvl (lvl - 1) r
onLvl lvl (Node _ l Empty) = onLvl (lvl - 1) l
onLvl lvl (Node _ l r)     = onLvl (lvl - 1) l ++ onLvl (lvl - 1) r

allCousins t = allCousins' t []
allCousins' Empty res          = res
allCousins' t@(Node _ l r) res = [(onLvl 2 t)] ++ (allCousins l) ++ (allCousins r)

sndCousines t node = if null nodeCousines then [] else node `delete` (head nodeCousines)
	where nodeCousines = filter (node `elem`) (allCousins t)

-- Task 4
type Battery = (Int, Double)
bestBattery :: [Battery] -> Int -> Double
bestBattery batteries k = minimum $ [price | (bk, price) <- batteries, bk >= k]

-- Task1B

listToFunction l = fun
	where fun x = if x `elem` l then x + 10 else 0

listToPredicate l = pred
	where pred x y = (x,y) `elem` l

-- Task circus
type Show = (String, Int, Int)

hourSpan mins = if mins `mod` 60 == 0 then mins `div` 60 else mins `div` 60 + 1 
end start len = (start + lenH, lenM)
	where
		lenH = len `div` 60
		lenM = len `mod` 60

trd (_, _, el) = el 
fst3 (el, _, _) = el

substractTime h1 (h2, m2) = (h2 * 60) + m2 - (h1 * 60)

showtime shows = ((overlapHour, substractTime overlapHour findOverlappingTime), map fst3 (snd maxOverlappingShows))
	where
		shows' = map (\(name, start, len) -> (name, start, end start len)) shows
		showAt (_, start, showEnd) h = h >= start && (h, 0) < showEnd
		hourOverlaps = map (\h -> (h, filter (`showAt` h) shows'))  [0..23] 
		maxOverlaps  = maximum $ map (length . snd) hourOverlaps
		maxOverlappingShows = head [ o | o@(_, shows) <- hourOverlaps, length shows == maxOverlaps]
		overlapHour = fst maxOverlappingShows
		findOverlappingTime = minimum $ map trd (snd maxOverlappingShows)

myScanr op nv []     = [nv]
myScanr op nv (x:xs) = x `op` head rest : rest
  where rest = myScanr op nv xs