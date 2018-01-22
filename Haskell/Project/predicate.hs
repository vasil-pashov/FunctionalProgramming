module Predicate (Predicate(Positive, Negative), areOpposite, negatePred, (~~), (/~~)) where
import Term

data Predicate = Positive String [Term] | Negative String [Term] deriving (Eq, Show)

areOpposite :: Predicate -> Predicate -> Bool
areOpposite (Negative p1 _) (Positive p2 _) = p1 == p2
areOpposite (Positive p1 _) (Negative p2 _) = p1 == p2
areOpposite _ _ = False

negatePred :: Predicate -> Predicate
negatePred (Positive name args) = (Negative name args)
negatePred (Negative name args) = (Positive name args)

(~~) :: Predicate -> Predicate -> Bool
(Positive pName pArgs) ~~ (Positive qName qArgs) = pArgs == qArgs && pName == qName 
(Negative pName pArgs) ~~ (Negative qName qArgs) = pArgs == qArgs && pName == qName 
(Negative pName pArgs) ~~ (Positive qName qArgs) = pArgs == qArgs && pName == qName 
(Positive pName pArgs) ~~ (Negative qName qArgs) = pArgs == qArgs && pName == qName 

(/~~) :: Predicate -> Predicate -> Bool
(/~~) p1 p2 = not $ p1 ~~ p2
