import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

{-
Term ::= Constant 
	       Variable
				 Predicate Term*
-}

data Term = Constant String
	        | Variable String
	        | Function String [Term]
	          deriving (Eq, Ord)

parseTerm :: String -> Either ParseError Term
parseTerm s = parse term "" s 

term :: Parser Term
term = do { try (char '?')
	        ; v <- many1 alphaNum
					; return (Variable v) 
          }
	 <|> do { try (char '(')
		      ; skipMany space
		      ; p <- many1 alphaNum 
					; skipMany space
		 		  ; xs <- term `sepEndBy` many space
					; char ')'
					; return (Function p xs)
		      }
   <|> do { c <- many1 alphaNum
			    ; return (Constant c)
					}

instance Show Term where
	show (Constant c) = c
	show (Variable v) = "?" ++ v
	show (Function p xs) = "( " ++ p ++ showTerms xs ++ " )" where 
		showTerms [] = ""
		showTerms (x:xs) = " " ++ show x ++ showTerms xs

{-
Artificial Intelligence a Modern Approach (3rd edition): 
Figure 9.1, page 398

function UNIFY(x, y, theta) returns a substitution to make x and y identical
	inputs: x, a variable, constant, list, or compound
	        y, a variable, constant, list, or compound
					theta, the substitution built up so far (optional, defaults to empty)

	if theta = failure then return failure
	else if x = y then return theta
	else if VARIABLE?(x) then return UNIFY-VAR(x, y, theta)
	else if VARIABLE?(y) then return UNIFY-VAR(y, x, theta)
	else if COMPOUND?(x) and COMPOUND?(y) then
		return UNIFY(x.args, y.args, UNIFY(x.OP, y.OP, theta))
	else if LIST?(x) and LIST?(y) then
		return UNIFY(x.REST, y.REST, UNIFY(x.FIRST, y.FIRST, theta))
	else return failure

function UNIFY-VAR(var, x, theta) returns a substitution
	inputs: var, a variable
          x, any expression
				  theta, the substitution built up so far

	if {var/val} \in theta then return UNIFY(val, x, theta)
	else if {x/val} \in theta then return UNIFY(var, val, theta)
	else if OCCUR-CHECK?(var, x) then return failure *
	else return add {var/x} to theta

	* Prolog omits this
-}

type Sub = Map.Map Term Term

unify :: Term -> Term -> Maybe Sub
unify x y = unify' x y (Just Map.empty) 

unify' :: Term -> Term -> Maybe Sub -> Maybe Sub
unify' _ _ Nothing = Nothing
unify' x y theta 
	| x == y = theta
unify' (Variable x) y theta = unifyVar (Variable x) y theta
unify' x (Variable y) theta = unifyVar (Variable y) x theta
unify' (Function p xs) (Function q ys) theta
	| (p /= q) || (length p /= length q) = Nothing
	| otherwise = foldl (\t (x,y) -> unify' x y t) theta (zip xs ys)  
unify' _ _ _ = Nothing

unifyVar :: Term -> Term -> Maybe Sub -> Maybe Sub
unifyVar var x (Just theta) 
	| Map.member var theta = case Map.lookup var theta of 
		(Just val) -> unify' val x (Just theta)
	|	Map.member x theta = case Map.lookup x theta of 
		(Just val) -> unify' x val (Just theta)
	| otherwise = (Just (Map.insert var x theta))
