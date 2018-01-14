-- Task 1
saySign :: (Num a, Eq a, Ord a) => a -> String
saySign x
  | x < 0      = "Negative"
  | x == 0     =  "Zeri"
  | otherwise  = "Positive"
    
-- Task 2
-- Exponential
fibonaci :: (Integral a) => a -> a
fibonaci 0 = 0
fibonaci 1 = 1
fibonaci n = fibonaci (n-1) + fibonaci(n-2)

--Iterative
fibonaci1 n = helper 0 1 1
  where helper a b i
          | i == n = b
          | otherwise = helper b (a+b) (i+1)

--Task3
countRoots :: (Num a, Eq a, Ord a) => a -> a -> a -> String
countRoots 0 0 c = "No Roots"
countRoots 0 b c = "One root"
countRoots a b c
  | d < 0       = "No roots"
  | d == 0      = "One root"
  | otherwise   = "Two roots"
  where d = 4*a*c - b^2

--Task 4
sayRoots :: (Num a, Eq a, Ord a) => a -> a -> a -> String
sayRoots 0 0 c = "No Roots"
sayRoots 0 b c
  | prod > 0  = "Positive"
  | prod == 0 = "Neutral"
  | otherwise = "Negative"
  where prod = (-b) * c
sayRoots a b c
  | prod < 0 = "Positive and negative"
  | prod  > 0 && c > 0 = "Both positive"
  | prod  > 0 && c < 0 = "Both Negative"
  | otherwise = "Neutral " ++ (sayRoots 0 a b)
  where prod = b * c

--Task 5
cylinderVolume :: Floating a => a -> a -> a
cylinderVolume r h = pi * h * r**2

-- Task 6
power :: (Num a, Integral b) => a -> b -> a
power _ 0 = 1
power x 1 = x
power x pow
  | even pow    = power x (pow `div` 2) ^ 2
  | otherwise = x * power x (pow - 1)