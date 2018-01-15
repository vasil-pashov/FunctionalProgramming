import Data.List (nub)
import GHC.Exts(sortWith)
-- Task 1
sumProducts :: Num t => [[t]] -> t
sumProducts = sum . map product

-- Task 2
occurrences :: Eq t => [t] -> [t] -> [Int]
occurrences a b = [count x b | x <- a]
  where count x = length . filter (==x)

-- Task 3
-- Assume that the matrix is square
mainDiag :: [[t]] -> [t]
mainDiag m = [(m !! i) !! i | i <- [0.. (length m) - 1]]

--Task 4
isSquare :: [[t]] -> Bool
isSquare m = and [(length row) == rows | row <- m]
  where rows = length m

-- Task 5
sndDiagonal :: [[t]] -> [t]
sndDiagonal m = [(m !! i) !! (len - i) | i <- [0.. len]]
  where len = length m - 1

-- Task 6
matchLengths :: [[t]] -> Bool
matchLengths [] = True
matchLengths (x:xs) = all (\ lst -> length lst == len) xs
  where len = length x

-- Task 7
setUnion :: (Eq t, Ord t) => [t] -> [t] -> [t]
setUnion [] set2 = set2
setUnion set1 [] = set1
setUnion set1@(x:xs) set2@(y:ys)
  | x <= y    = x:setUnion xs set2
  | otherwise = y:setUnion set1 ys

setIntersect :: (Eq t, Ord t) => [t] -> [t] -> [t]
setIntersect a b = [x | x <- a, x `elem` b]

setDifference :: (Eq t, Ord t) => [t] -> [t] -> [t]
setDifference a b = [x | x <- a, not (x `elem` b)]

--Task 8

--Task 9
cntOccurrences :: Eq t => t -> [t] -> Int
cntOccurrences x = length . filter (==x)

mostCommon :: (Eq t, Ord t) => [t] -> t
mostCommon lst = fst $ (foldr1 maxEl hist)
  where 
    uniques = nub lst
    hist    = [(el, cntOccurrences el lst) | el <- uniques]
    maxEl x@(x1, x2) y@(y1, y2)
      | x2 > y2             = x
      | x2 == y2 && x1 > y1 = x
      | otherwise           = y

specialSort :: (Ord t, Eq t) => [[t]] -> [[t]]
specialSort = sortWith mostCommon