-- Task 1
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose m = map head m : transpose (map tail m)

colInAnyRow :: Eq a => [[a]] -> [a] -> Bool
colInAnyRow m col = length (filter (`colInRow` col) m) > 0

colInRow ::Eq a => [a] -> [a] -> Bool
colInRow row col = all (`elem` row) col

findColumns m = let t = transpose m 
				in length $ filter (m `colInAnyRow`) t
				
-- Task 2
combine :: Num a => (a -> a) -> (a -> a) -> (a -> a -> a) -> a -> a
combine f g h x = h (f x) (g x) 

--check :: (Eq a, Num a) => Int -> Int -> [(a -> a)] -> [(a -> a -> a)] -> Bool
check a b uns bins = or [ (composedRes f g h) == (unRes t ) | f <- uns, g <- uns, h <- bins, t <- uns]
  where composedRes f g h = map (combine f g h) [a..b]
        unRes t           = map t [a..b]

-- Task 3
type Plant = (String, Int, Int)

plant :: Plant -> String
plant (res, _, _) = res

plantMinTemp  :: Plant -> Int
plantMinTemp (_, res, _) = res

plantMaxTemp  :: Plant -> Int
plantMaxTemp (_, _, res) = res

minTemp :: [Plant] -> Int
minTemp = foldr1 min . map plantMinTemp

maxTemp :: [Plant] -> Int
maxTemp = foldr1 max . map plantMaxTemp

canSurvive :: Int -> Int -> Plant -> Bool
canSurvive min max p = pMin <= min && pMax >= max
  where pMin = plantMinTemp p
        pMax = plantMaxTemp p

--garden :: [Plant] -> [Plant]
garden p = (fst maxSurvie, map plant (snd maxSurvie))
  where minT = minTemp p
        maxT = maxTemp p
        surviving a b = filter (canSurvive a b) p
        largerLst l1 l2
          | length (snd l1) >= length (snd l2) = l1
		  | otherwise                          = l2
        maxSurvie = foldr1 largerLst [((a,b), surviving a b) | a <- [minT..maxT], b <- [a..maxT]]

-- Task 4

next :: Eq t => t -> [[t]] -> [t]
next _ [] = []
next node ((n:ch):rest)
  | node == n = ch
  | otherwise = next node rest 

extendPath :: Eq t => [t] -> [[t]] -> [[t]]
extendPath p@(currnet:rest) graph = foldr extend' [] (next currnet graph) 
  where extend' node res
          | not (node `elem` p) = ((node:p):res)
		  | otherwise           = res
--maxPath :: (Eq t, Ord t) => [t] -> [[t]] -> Int
maxPath from g = maxPath' (extendPath [from] g) [from]
	where
		maxPath' [] res = res
		maxPath' lvl@(x:_) _ = maxPath' (nextLvl lvl) x 
			where nextLvl lvl = concat $ map (`extendPath` g) lvl