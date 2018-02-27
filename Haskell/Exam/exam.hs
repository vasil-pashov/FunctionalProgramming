--Task 1

largestInterval f g a b = findInterval prefLengths
	where
		[fVals, gVals] = map (`map` [a..b]) [f,g]
		longestPrefixLen [] [] = 0
		longestPrefixLen (x:xs) (y:ys)
			| x == y = 1 + longestPrefixLen xs ys
			| otherwise = 0
		prefLengths = [longestPrefixLen (i `drop` fVals) (i `drop` gVals) | i <- [0..length fVals]]
		findInterval lengths = findInterval' lengths a (-1) a
		findInterval' [] start len _ = (start, start + len - 1)
		findInterval' (len:lens) start maxLen current
			| len > maxLen = findInterval' lens current len next
			| otherwise = findInterval' lens start maxLen next
				where next = current + 1

-- Task2 
data Tree t = Empty | Node t (Tree t) (Tree t) deriving (Show, Eq)

find _  (Node root Empty Empty) = root
find op (Node root left Empty)  = root `op` (find op left)
find op (Node root Empty right) = root `op` (find op right)
find op (Node root left right) = root `op` (leftRes `op` rightRes)
  where
	leftRes = find op left
	rightRes = find op right

intervalTree :: (Ord t, Eq t) => (Tree t) -> (Tree (t, t))
intervalTree Empty = Empty
intervalTree t@(Node root left right) = (Node (find min t, find max t) (intervalTree left) (intervalTree right))

-- Task 3

sumOfSquares = [ x^2 + y^2 | x <- [1..], y <- [1..x] ]

--Task 4

type Video = (String, Int)

average videos = fromIntegral((sum (map snd videos)))/ (fromIntegral ((length videos)))

averageVideo :: [Video] -> String
averageVideo videos =  fst $ fst findClosest
  where
	avg = average videos
	dists = [(v, dist) | v@(_, len) <- videos, let dist = avg - fromIntegral len, dist > 0]
	findClosest = foldr1 (\v@(_, dist) v1@(_, dist1) -> if dist < dist1 then v else v1) dists
	

fullBinary :: Int -> Tree Int
fullBinary h = fullBinary' h
  where fullBinary' 0  = Empty
        fullBinary' h' = Node h (fullBinary' (h' - 1)) (fullBinary' (h' - 1))

treeStream :: [Tree Int]
treeStream = [fullBinary i| i <- [0..]]