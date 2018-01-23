import Data.List (delete, nub)
import Term
import Predicate
import Disjunct
import Unifiable
import Substitutable

type Program = [Disjunct]

program::Program
-- program = [
-- 	(Disjunct [Positive "p" [Const "10"]]),
-- 	(Disjunct [Positive "p" [Const "11"]]),
-- 	(Disjunct [Positive "p" [Var "X"], Negative "q" [Var "X"]]),
-- 	(Disjunct [Positive "q" [Const "15"]])]
program = [
	(Disjunct [Positive "p" [Const "1", Const "2"]]),
	(Disjunct [Positive "p" [Var "X", Var "Y"], Negative "q" [Var "X"], Negative "r" [Var "Y"]]),
	(Disjunct [Positive "q" [Const "10"]]),
	(Disjunct [Positive "q" [Var "YY"], Negative "t" [Var "YY"], Negative "r" [Const "11"]]),
	(Disjunct [Positive "r" [Const "11"]]),
	(Disjunct [Positive "t" [Const "5"]])]

goal::Disjunct
-- goal = (Disjunct [Negative "p" [Var "X"]])
goal = (Disjunct [Negative "p" [Var "X", Var "Y"]])
	 
t = (Function "f" [(Const "x"), (Var "X"), (Const "z")])
t1 = (Function "f" [(Const "x"), (Var "Z"), (Var "X")]) 
t2 = (Function "f" [(Const "x"), (Const "z"), (Var "X")]) 
-----------------------------------------------------------------------------

equalSet :: Eq t => [t] -> [t] -> Bool
equalSet x y
	| length x /= length y = False
	| otherwise            = all (`elem` y) x


myp = [Disjunct [Positive "p" [Const "11"]], Disjunct [Positive "p" [Var "X"], Negative "p" [Var "X"]]]
myg = (Disjunct [Negative "p" [Var "X"]])

--resolve :: Program -> Disjunct -> [[(Term, Term)]]
resolve program goal = processLevel $ nextLvl goal 
	where
		nextLvl = (program `newGoals`)
		processLevel :: [(Disjunct, [(Term, Term)])] -> [[(Term, Term)]]
		processLevel []                         = []
		processLevel ((EmptyDisjunct, subs):gs) = subs : processLevel gs
		processLevel ((d, subs):gs) = restOfLvl ++ (map (++subs) $ processLevel $ nextLvl d) 
			where restOfLvl = processLevel gs

newGoals :: Program -> Disjunct -> [(Disjunct, [(Term, Term)])]
newGoals program goal = resolved
	where
		opposites = zip program (map (goal `pairOpposites`) program)
		resolved = takeResolvents $ concat $ map unifyOpposites opposites
		unifyOpposites (d, lstOpp) = [(subs, d, p1, p2) | (p1, p2) <- lstOpp, let subs = p1 `unify` p2,
																					subs /= Nothing]	
		takeResolvents = map (\(s@(Just (_, subs)), d, p1, p2) -> (takeResolvent goal d p1 p2 s, subs))
