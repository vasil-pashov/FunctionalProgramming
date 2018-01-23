import Term

data Token = TVar String | TId String | Openning | Closing | Dot | Implication | Comma deriving (Show, Eq)

data PStruct = PConst String 
				| PVar String 
				| PTerm String [PStruct] 
				| PAtom String PStruct 
				| PFact PStruct
				| PImpl PStruct [PStruct] deriving (Show)

removeWhitespace :: String -> String
removeWhitespace = filter (not . (`elem` whitespace))
	where whitespace = ['\n', ' ', '\t']

readAlphanumeric :: String -> (String, String)
readAlphanumeric input = (identifier, length identifier `drop` input)
	where
		canTake = (`elem` (['a'..'z'] ++ ['1'..'9'] ++ ['A'..'Z']))
		identifier = takeWhile canTake input

isVariable :: String -> Bool
isVariable (c:_) = c `elem` ['A'..'Z']

isIdentifier :: String -> Bool
isIdentifier (c:_) = c `elem` ['a'..'z']

tokenize :: String -> [Token]
tokenize [] = []
tokenize input@(h:t)
	| isVariable input   = TVar item : tokenize rest 
	| isIdentifier input = TId item : tokenize rest
	| '(' == h  	 	 = Openning : (tokenize t)
	| ')' == h  	 	 = Closing : (tokenize  t)
	| ':' == h  	 	 = Implication : (tokenize $ tail t)
	| '.' == h  	 	 = Dot : (tokenize t)
	| ',' == h  	 	 = tokenize t
		where (item, rest) = readAlphanumeric input

structureTokens :: [Token] -> [PStruct]
structureTokens tokens = reverse $ structureTokens' tokens []
	where
		structureTokens' [] stack                 = stack
		structureTokens' (Dot:rest) (h:t)         = structureTokens' rest ((PFact h):t)   
		structureTokens' (Implication:rest) (h:t) = structureTokens' next (impl:t)
			where
				(implPreCond, next) = parseImplicationPreCond rest
				impl = PImpl h implPreCond
		structureTokens' tokens stack = structureTokens' rest (term:stack)
			where (term, rest) = parseTerm tokens


parseImplicationPreCond (Dot:t) = ([], t)
parseImplicationPreCond tokens@(TId name:t) = (term:args, next)
	where
		(term, rest) = parseTerm tokens
		(args, next) = parseImplicationPreCond rest

parseTerm (TId name:Openning:t) = (PTerm name args, rest)
	where (args, rest) = parseTermArgs t

parseTermArgs tokens = case tokens of
						Closing:t              -> ([], t)
						(TVar name:_)          -> ((PVar name):args, r)
						(TId name:Openning:_) -> (term:args, r1)
						(TId name:_)           -> (PConst name:args, r) 
						where
							(args, r)  = parseTermArgs $ tail tokens
							(term, r1) = parseTerm tokens

main = do
		file <- readFile "test.pl"
		print $ removeWhitespace file
		let tokenized = (tokenize $ removeWhitespace file)
		print tokenized