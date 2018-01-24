import Intrepreter
import Disjunct
import Term
import Predicate

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

parseTermArgs tokens = parseTermArgs' tokens []
	where
		parseTermArgs' tokens args = case tokens of
										(Closing:t)            -> (reverse args, t)
										(TVar name:t)          -> parseTermArgs' t (PVar name:args)
										(TId name:Openning:t)  -> parseTermArgs' next (term:args)
										(TId name:t)           -> parseTermArgs' t (PConst name:args) 
										where (term, next) = parseTerm tokens

structuredTokensToProgram = map parseStruct
	where
		parseStruct fact@(PFact _) = factToDisjunct fact
		parseStruct impl@(PImpl _ _) = implicationToDisjunct impl



factToDisjunct (PFact term) = (Disjunct [termToPredicate Positive term])

implicationToDisjunct (PImpl res preConds) = (Disjunct ((termToPredicate Positive res):preCondPred))
	where preCondPred = map (termToPredicate Negative) preConds

queryToDisjunct query = (Disjunct $ map (termToPredicate Negative) structured)
	where structured = structureTokens $ tokenize $ removeWhitespace query

termToPredicate sign (PTerm name args) = (sign name (parseArgs args))
	where
		parseArgs [] = []	
		parseArgs ((PVar name):t)       = (Var name):parseArgs t
		parseArgs ((PConst name):t)     = (Const name):parseArgs t
		parseArgs ((PTerm name args):t) = (Function name (parseArgs args)):parseArgs t

main = do
		putStrLn "Enter file to consult"
		filename <- getLine
		file <- readFile filename
		-- print $ removeWhitespace file
		let program = structuredTokensToProgram $ structureTokens $ (tokenize $ removeWhitespace file)
		mapM_ print program
		readQuery program
		
readQuery program = do
				putStr "?- "
				query <- getLine
				if query == ":q"
					then return "Exiting"
					else do
							let
								parsedQ = queryToDisjunct query
								resolved = resolve program parsedQ
							printResolved resolved	
							readQuery program

printResolved []   = putStrLn (show False)
printResolved [[]] = putStrLn (show True)
printResolved sth  = do
						putStr $ show (head sth)
						next <- getLine
						if null next
							then printResolved $ tail sth
							else return ()

