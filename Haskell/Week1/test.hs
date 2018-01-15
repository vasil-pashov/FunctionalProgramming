divides :: Integral t => t -> t -> Bool
divides x y = (y `mod` x) == 0

eratostenesPrimes :: Integral t => [t]
eratostenesPrimes = helper [2..]
  where helper (x:xs) = x:helper (filter (not . (x `divides`)) xs)