module Disjunct(Disjunct(EmptyDisjunct, Disjunct), pairOpposites, takeResolvent) where
import Substitutable
import Predicate
import Data.List (concat, delete, nub)

data Disjunct = EmptyDisjunct | Disjunct [Predicate] deriving (Show, Eq)

disjunctFromSet :: [Predicate] -> Disjunct
disjunctFromSet [] = EmptyDisjunct
disjunctFromSet set = Disjunct set

findOpposites :: Predicate -> Disjunct -> [Predicate]
findOpposites pred (Disjunct d) = filter (`areOpposite` pred) d

--pairOpposites :: Disjunct -> Disjunct -> [(Predicate, Predicate)]
pairOpposites (Disjunct d1) (Disjunct d2) = [(a, b) | a <- d1, b <- d2, a `areOpposite` b]

takeResolvent (Disjunct d1) (Disjunct d2) p1 p2 subs = unifySubstituted substituted 
--	where joined = disjunctFromSet $ nub $ p1 `delete` d1 ++ p2 `delete` d2
	where
		joined = disjunctFromSet (d1++d2)
		substituted = substituteList joined subs
		unifySubstituted (Disjunct d) = disjunctFromSet $ removeUnified $ nub d
		removeUnified = filter (\pred -> pred /~~ p1 && pred /~~ p2)

instance Substitutable Disjunct where
	substitute _ _ EmptyDisjunct           = EmptyDisjunct
	substitute var newVal (Disjunct preds) = (Disjunct (map (substitute var newVal) preds))

