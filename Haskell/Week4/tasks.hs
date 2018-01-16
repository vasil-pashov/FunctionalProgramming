--Task 1
type Point = (Double, Double)
maxDistance :: (Point -> Point -> Double) -> [Point] -> Double
maxDistance fn pts = maximum [fn p1 p2 | p1 <- pts, p2 <- pts ]

--Task 2
type Item = (String, Integer)
expiringItems :: [Item] ->  (String, Integer, String)
expiringItems items = (nextToExpire, expiredCnt, oldestExpired)
  where
   (expired, expiringToday, notExpired)   = split snd items
   nextToExpire  = fst $ foldr1 (cmp (<)) (notExpired ++ expiringToday)
   expiredCnt    = toInteger $ length expired
   oldestExpired = fst $ foldr1 (cmp (<)) expired
   cmp comparator a@(x1, y1) b@(x2, y2)
     | y1 `comparator` y2  = a
     | otherwise = b



split fn lst = (negatives, zeros, positives)
  where
    negatives = filter ((<0).fn) lst
    zeros     = filter ((==0).fn) lst
    positives = filter ((>0).fn) lst

--Task 3
data Tree a = Empty | Node {root::a, left::Tree a, right::Tree a} deriving (Eq, Show, Read)

makeLeaf :: t -> Tree t
makeLeaf x = Node x Empty Empty

isLeaf :: Tree t -> Bool
isLeaf (Node _ Empty Empty) = True
isLeaf (Node _ _ _) = False

addLeft :: x -> Tree x -> Tree x
x `addLeft` t = t{right=makeLeaf x}

addRight :: x -> Tree x -> Tree x
x `addRight` t = t{left=makeLeaf x}

maxPathSum :: (Num t, Ord t) => Tree t -> t
maxPathSum Empty = 0
maxPathSum (Node root l r) = root + max (maxPathSum l) (maxPathSum r)

-- Task 4

prune :: Tree t -> Tree t
prune t@Node{left=l, right=r}
  | isLeaf t  = Empty
  | otherwise = t {left=prune l, right=prune r}

-- Task 5

bloom :: Tree t -> Tree t
bloom t@Node{root=root, left=l, right=r}
  | isLeaf t  = t {left=makeLeaf root, right=makeLeaf root}
  | otherwise = t {left=bloom l, right=bloom r}

-- Task 6

type BST a = Tree a

bstinsert :: (Eq a, Ord a) => a -> BST a -> BST a
bstinsert x Empty = makeLeaf x
bstinsert x t@Node{root=root, left=l, right=r}
  | x == root = t
  | x > root = t {right=bstinsert x r}
  | x < root = t {left=bstinsert x l}

bstsearch :: (Eq a, Ord a) => a -> BST a -> Bool
bstsearch _ Empty = False
bstsearch x t
  | x == root t = True
  | otherwise   = x `bstsearch` (left t) || x `bstsearch` (right t)

bstvalues :: BST a -> [a]
bstvalues Empty = []
bstvalues (Node root Empty Empty) = [root]
bstvalues (Node root left Empty) = bstvalues left ++ [root]
bstvalues (Node root Empty right) = root : bstvalues right
bstvalues (Node root left right) = bstvalues left ++ [root] ++ bstvalues right

bstsize :: BST a -> Integer
bstsize = toInteger . length . bstvalues


