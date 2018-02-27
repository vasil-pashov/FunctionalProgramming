module Substitutable (Substitutable, substitute, substituteList, compose) where
import Term
import Predicate

type Substitution = [(Term, Term)]

class Substitutable a where
  substitute :: Term -> Term -> a -> a
  substituteList :: a -> [(Term, Term)] -> a
  substituteList obj subs = foldr (\(from, to) acc -> substitute from to acc) obj subs

instance Substitutable Term where
  substitute (Var v) newVal init@(Var v1)
    | v == v1 = newVal
    | otherwise = init
  substitute (Var v) _ c@(Const _) = c
  substitute var@(Var _) newVal (Function name args) = (Function name (substituteArgs args))
    where substituteArgs = map (substitute var newVal)

instance Substitutable Predicate where
  substitute var newVal (Positive name terms) = (Positive name (map (substitute var newVal) terms))
  substitute var newVal (Negative name terms) = (Negative name (map (substitute var newVal) terms))

compose :: Substitution -> Substitution
compose [] = []
compose ((from, to):rest) = if boudToVar
                            then compose substituteVar
                            else (from,to):compose rest
  where
    boudToVar = any ((==from) . snd) rest
    substituteVar = map (\(f, t) -> (f, if t == from then to else t)) rest