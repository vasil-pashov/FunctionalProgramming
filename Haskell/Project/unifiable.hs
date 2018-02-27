module Unifiable(unify, applySubs, getVars) where
import Data.List(nub)
import Term
import Predicate
import Substitutable

type Substitution = (Term, Term)

class Unifiable a where
  unify :: a -> a -> Maybe (a, [Substitution])
  getVars :: a -> [String]

instance Unifiable Term where
  unify a b
    | a == b    = Just (a, [])
    | otherwise = case nextSubstitution a b of
            Nothing -> Nothing
            Just subs@[(from, to)] -> case (substitute from to a) `unify` (substitute from to b) of
                        Nothing -> Nothing
                        Just (t, r) -> Just (t, r ++ subs)
    where
      nextSubstitution v@(Var a) v1@(Var b)
        | a == b = Just []
        | otherwise = Just [(v,v1)]
      nextSubstitution v@(Var _) c@(Const _) = Just [(v,c)]
      nextSubstitution c@(Const _) v@(Var _) = Just [(v,c)]
      nextSubstitution (Const _) (Function _ _) = Nothing
      nextSubstitution (Function _ _) (Const _) = Nothing
      nextSubstitution (Const a) (Const b)
          | a == b    = Just []
        | otherwise = Nothing
      nextSubstitution f@(Function _ _) v@(Var _) = nextSubstitution v f
      nextSubstitution v@(Var _) f@(Function _ _)
        | f `contains` v = Nothing
        | otherwise      = Just [(v, f)]
      nextSubstitution f@(Function name1 args1) g@(Function name2 args2)
        | name1 /= name2               = Nothing
        | length args1 /= length args2 = Nothing
        | otherwise      = nextSubstitutionArgs args1 args2
        where
          nextSubstitutionArgs [] [] = Just []
          nextSubstitutionArgs (a:as) (b:bs) = case a `nextSubstitution` b of
                        Nothing -> Nothing
                        Just [] -> as `nextSubstitutionArgs` bs
                        Just [subs] -> Just [subs]

  getVars (Const _) = []
  getVars (Var name) = [name]
  getVars (Function _ terms) = concatMap getVars terms


--applySubs :: (Substitutable t) => [Substitution] -> t -> t
applySubs = foldr (\ (from, to) res -> substitute from to res)

instance Unifiable Predicate where
  unify p@(Negative _ _) q@(Positive _ _) = unify q p
  unify p@(Positive pName pArgs) q@(Negative qName qArgs)
    | pName /= qName               = Nothing
    | length pArgs /= length qArgs = Nothing
    | pArgs == qArgs               = Just (p, [])
    | otherwise                    = unifyTerms $ zip qArgs pArgs
    where
      unifyTerms :: [(Term, Term)] -> Maybe (Predicate, [Substitution])
      unifyTerms [] = Nothing
      unifyTerms ((t1,t2):ts)
        | t1 == t2  = unifyTerms ts
        | otherwise = case t1 `unify` t2 of
                Nothing -> Nothing
                Just (_, subsChain) -> case newP `unify` newQ of
                            Nothing -> Nothing
                            Just (resPred, subs) -> Just (resPred, subs ++ subsChain) 
                  where
                    newP = p `applySubs` subsChain
                    newQ = q `applySubs` subsChain

  getVars d = case d of
              (Positive _ t) -> getVars' t
              (Negative _ t) -> getVars' t
    where getVars' = nub . concatMap getVars

