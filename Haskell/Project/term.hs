module Term (Term(Const,Var,Function), contains) where

data Term = Const String | Var String | Function String [Term] deriving (Show, Read, Eq)
		
contains :: Term -> Term -> Bool
contains (Function _ args) v@(Var _) = contains' args
	where
		contains' [] = False
		contains' (v1@(Var _):ts)
			| v == v1   = True
			| otherwise = contains' ts 
		contains' ((Const _):ts) = contains' ts
		contains' (f@(Function _ _):ts)
			| contains f v = True
			| otherwise    = contains' ts