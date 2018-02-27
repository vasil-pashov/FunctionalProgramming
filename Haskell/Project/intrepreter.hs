module Intrepreter where
import Data.List (delete, nub, intersect)
import Control.Arrow
import Term
import Data.Maybe
import Predicate
import Disjunct
import Unifiable
import Substitutable

type Program = [Disjunct]


equalSet :: Eq t => [t] -> [t] -> Bool
equalSet x y
  | length x /= length y = False
  | otherwise            = all (`elem` y) x

resolve :: Program -> Disjunct -> [[(Term, Term)]]
resolve program goal = processLevel $ newGoals [] program goal 
  where
    processLevel :: [(Disjunct, [(Term, Term)])] -> [[(Term, Term)]]
    processLevel []                         = []
    processLevel ((EmptyDisjunct, subs):gs) = subs:processLevel gs
    processLevel ((d, subs):gs) = restOfLvl ++ [newSubs | s <- processLevel $ newGoals (map fst subs) program d,
                                                        let newSubs = subs ++ s,
                                                        not (newSubs `hasEqualSet` restOfLvl)]
      where restOfLvl = processLevel gs

hasEqualSet :: [(Term, Term)] -> [[(Term, Term)]] -> Bool
hasEqualSet set = any (set `equalSet`)

newGoals :: [Term] -> Program -> Disjunct -> [(Disjunct, [(Term, Term)])]
newGoals used program goalInit = map (Control.Arrow.second (updates ++)) resolved
  where
    (goal, updates) = prepareGoal used program goalInit
    opposites = zip program (map (goal `pairOpposites`) program)
    resolved = takeResolvents $ concatMap unifyOpposites opposites
    unifyOpposites (d, lstOpp) = [(subs, d, p1, p2) | (p1, p2) <- lstOpp,
                                                      let subs = p1 `unify` p2,
                                                      Data.Maybe.isJust subs]  
    takeResolvents = map (\(s@(Just (_, subs)), d, p1, p2) -> (takeResolvent goal d p1 p2 s, subs))


prepareGoal :: [Term] -> Program -> Disjunct -> (Disjunct, [(Term, Term)])
prepareGoal usedVars program goal = foldr update (goal, []) toChange
  where disjVars (Disjunct p) = concatMap getVars p
        programVars = concatMap disjVars program
        used = map (\(Var name) -> name) usedVars
        goalVars = disjVars goal
        forbidenNames =  used ++ programVars
        toChange = goalVars `intersect` (programVars ++ used)
        newVarName name goal = if name `notElem` forbidenNames && name `notElem` disjVars goal
                               then name
                               else newVarName (nextName name) goal
        update name (d, subs) = (substitute oldVar newVar d, (oldVar, newVar):subs)
          where oldVar = Var name
                newVar = Var (newVarName name d)

nextName :: String -> String
nextName ('Z':t) = 'A':'Z':t
nextName (h:t)   = succ h:t

p = [Disjunct [Negative "c" [Const "x"]],
    Disjunct [Negative "c" [Var "X"], Positive "c" [Function "f" [Var "X"]]]]
g = Disjunct [Negative "c" [Var "X"]]