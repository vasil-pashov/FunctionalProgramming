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