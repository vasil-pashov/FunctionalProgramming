module Disjunct(Disjunct(EmptyDisjunct, Disjunct), pairOpposites, takeResolvent) where
import Substitutable
import Predicate
import Term
import Data.List (concat, delete, nub)

data Disjunct = EmptyDisjunct | Disjunct [Predicate] deriving (Show, Eq)

disjunctFromSet :: [Predicate] -> Disjunct
disjunctFromSet [] = EmptyDisjunct
disjunctFromSet set = Disjunct set

findOpposites :: Predicate -> Disjunct -> [Predicate]
findOpposites pred (Disjunct d) = filter (`areOpposite` pred) d

--pairOpposites :: Disjunct -> Disjunct -> [(Predicate, Predicate)]
pairOpposites (Disjunct d1) (Disjunct d2) = [(a, b) | a <- d1, b <- d2, a `areOpposite` b]

joinDisjuncts (Disjunct d1) (Disjunct d2) = disjunctFromSet $ nub $ d1 ++ d2
joinDisjuncts EmptyDisjunct d = d
joinDisjuncts d EmptyDisjunct = d

takeResolvent :: Disjunct -> Disjunct -> Predicate -> Predicate -> Maybe (Predicate, [(Term, Term)]) -> Disjunct
takeResolvent d1 d2 p1 p2 (Just (res, subs)) = joinDisjuncts newD1 newD2
  where
    newD1 = (substituteList d1 subs) `remove` (substituteList p1 subs)
    newD2 = (substituteList d2 subs)  `remove` (substituteList p2 subs)

remove :: Disjunct -> Predicate -> Disjunct
remove (Disjunct d) p = disjunctFromSet $ p `delete` d

instance Substitutable Disjunct where
  substitute _ _ EmptyDisjunct           = EmptyDisjunct
  substitute var newVal (Disjunct preds) = Disjunct $ nub $ map (substitute var newVal) preds

