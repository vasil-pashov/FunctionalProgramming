--util funcs
--True if x devides y
divides :: Integral t => t -> t -> Bool
divides x y = (y `mod` x) == 0

--Task 1
gcd' :: (Integral a, Eq a) => a -> a -> a
gcd' a 0 = a
gcd' 0 b = b
gcd' a b
  | a == b    = a
  | a > b     = gcd' (a `mod` b) b
  | otherwise = gcd' a (b `mod` a)

--Task 2 
akkerman :: Integral a => a -> a -> a
akkerman 0 n = n + 1
akkerman m 0 = akkerman (m-1) 1
akkerman m n = akkerman (m-1) (akkerman m (n-1))

--Task 3
modulus (a, b) = sqrt (a^2 + b^2)

--Task 4
complAdd (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

complSub (a1, b1) (a2, b2) = (a1 - a2, b1 - b2)

complMult (a, b) (c, d) = (a * c - b * d, a * d + b * c)

--Task 5
distance :: Floating a => (a,a) -> (a,a) -> a
distance (x1, y1) (x2, y2) = sqrt((x1 - x2)^2 + (y1 - y2)^2)

--Task 6
replicate' :: (Integral t0) => t0 -> t -> [t]
replicate' cnt el = [el | _ <- [1..cnt]]

replicate'' :: (Integral t0) => t0 -> t -> [t]
replicate'' 0 _ = []
replicate'' cnt el = el:replicate'' (cnt-1) el

replicate3' :: (Integral t0) => t0 -> t -> [t]
replicate3' cnt el = map (\_->el) [1..cnt] 


--Task 7
take' :: Int -> [t] -> [t]
take' cnt lst = [lst !! x | x <- [0..l-1]]
  where l = min cnt (length lst)

--Task 8
map' :: (t0 -> t1) -> [t0] -> [t1]
map' fn lst = [fn x | x <- lst]

filter' :: (t0 -> Bool) -> [t0] -> [t0]
filter' fn lst = [x | x <- lst, fn x]

divisors :: Integral t => t -> [t]
divisors n = filter' (`divides` n) [1..n]
  where divides x y = y `mod` x == 0

divisorsSum :: Integral t => t -> t
divisorsSum = sum  . divisors

divisorsCnt :: Integral t => t -> Int
divisorsCnt = length . divisors

isPrime :: Integral t => t -> Bool
isPrime = (==2) . divisorsCnt

descartes :: Num t => [t] -> [t] -> [(t,t)]
descartes lst1 lst2 = [(x,y) | x <- lst1, y <- lst2]

--Task 9
primes :: Integral t => [t]
primes = [x | x <- [1..], isPrime x]

--Task 10
eratostenesPrimes :: Integral t => [t]
eratostenesPrimes = helper [2..]
  where helper (x:xs) = x:helper (filter (not . (x `divides`)) xs)

--Task 11
flip' :: (t -> t1 -> t2) -> (t1 -> t -> t2)
flip' fn a b = fn b a

--Task12
takeWhile' :: (t -> Bool) -> [t] -> [t]
takeWhile' _ [] = []
takeWhile' pred (x:xs)
  | pred x    = x:takeWhile' pred xs
  | otherwise = [] 

takeWhile'' :: (t -> Bool) -> [t] -> [t]
takeWhile'' pred [] = []
takeWhile'' pred lst = reverse $ helper lst []
  where helper lst res
          | null lst = []
          | pred x = helper xs (x:res)
          | otherwise = res
            where (x:xs) = lst
			
--Task 13
compress :: Eq t => [t] ->[(t,Int)]
compress [] = []
compress lst@(x:_) = (x,len):compress (drop len lst)
  where len = length (takeWhile' (==x) lst)

-- Task 14
maxRepeated :: Eq t => [t] -> Int
maxRepeated = (foldr (\(_, len) res -> max len res) 0) . compress

-- Task 15
makeSet :: Eq t => [t] -> [t]
makeSet lst = makeSet' lst []
  where makeSet' lst res
		  | null lst     = res
		  | x `elem` res = makeSet' xs res
		  | otherwise    = makeSet' xs (x:res)
		    where (x:xs) = lst

-- Task 16
histogram :: Eq t => [t] -> [(t, Int)]
histogram lst = [(x, count x lst) | x <- makeSet lst]
  where count x = length . filter (==x) 

-- Task 17
maxDistance :: [(Double, Double)] -> Double
maxDistance pts = maximum [distance a b | a <- pts, b <- pts]

-- Task 18
--compositions :: (t -> t1) -> [(t->t1)]
compositions f = [repeatN n | n <- [1..]]
  where repeatN n = foldr1 (.) (replicate n f)
 