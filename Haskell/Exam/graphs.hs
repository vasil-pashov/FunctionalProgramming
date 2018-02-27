type Graph t = [[t]]

g = [[1,3,2],
    [2,3,6],
    [3,4,6],
    [4,5,1],
    [5,3],
    [6,5]]

assoc :: Eq t => t -> [[t]] -> [t]
assoc el = foldr (\x res -> if head x == el then tail x else res) []

nodes :: Graph t -> [t]
nodes = map head

children ::Eq t => t -> Graph t -> [t]
children = assoc

parents :: Eq t => t -> Graph t -> [t]
parents el g = [node | (node:sons) <- g, el `elem` sons]

hasEdge :: Eq t => t -> t -> Graph t -> Bool
hasEdge u v = (v `elem`) . (u `children`)

symmetric :: Eq t => Graph t -> Bool
symmetric g = all (\node -> all (\child -> hasEdge child node g) (children node g)) $ nodes g

dfsPath :: Eq t => t -> t -> Graph t -> [t]
dfsPath start end g = dfsPath' start [start]
  where dfsPath' node path
          | node == end = reverse path
          | null next = []
          | null results = []
          | otherwise = head results
          where next    = filter (not . (`elem` path)) $ children node g
                results = [res | son <- next, let res = dfsPath' son (son:path), not $ null res ]

allPathsDfs start end g = allPaths' start [start]
  where allPaths' node path
          | node == end = [reverse path]
          | null next   = []
          | otherwise   = results
          where next    = filter (not . (`elem` path)) $ children node g
                results = [res | son <- next,
                                 let resPaths = allPaths' son (son:path),
                                  res <- filter (not . null) resPaths ]

bfsPath :: Eq t => t -> t -> Graph t -> [t]
bfsPath start end g = reverse $ bfsPath' [[start]]
  where possiblePaths current@(h:_) = [child:current | child <- children h g, child `notElem` current]
        nextLvl = concatMap possiblePaths
        bfsPath' [] = []
        bfsPath' lvl
          | not $ null res = head res
          | otherwise      = bfsPath' $ nextLvl lvl
          where res = filter ((==end).head) lvl