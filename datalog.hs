import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

-- Term ::= Constant 
--          Variable
--          Predicate Term*

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

-- Artificial Intelligence a Modern Approach (3rd edition):
-- TODO: Copy the definition of substitutions

data Sub = Sub (Map.Map Term Term)

instance Show Sub where
  show (Sub m) = "{" ++ showSubs (Map.toList m) ++ " }" where
    showSubs [] = ""
    showSubs ((v,t):ss) = " " ++ show v ++ "/" ++ show t ++ showSubs ss

-- Artificial Intelligence a Modern Approach (3rd edition):
-- (Text), page 323

subst :: Term -> Sub -> Term
subst (Constant c) _ = (Constant c)
subst (Variable v) (Sub m) = case Map.lookup (Variable v) m of
  (Just t) -> subst t (Sub m)
  Nothing -> (Variable v)
subst (Function p xs) theta = (Function p (substTerms xs theta)) where
  substTerms [] _ = []
  substTerms (x:xs) theta = subst x theta : substTerms xs theta

-- Artificial Intelligence a Modern Approach (3rd edition): 
-- Figure 9.1, page 328
--
-- function UNIFY(x, y, theta) returns a substitution to make x and y identical
-- inputs: x, a variable, constant, list, or compound
--         y, a variable, constant, list, or compound
--         theta, the substitution built up so far (optional, defaults to empty)
--
-- if theta = failure then return failure
-- else if x = y then return theta
-- else if VARIABLE?(x) then return UNIFY-VAR(x, y, theta)
-- else if VARIABLE?(y) then return UNIFY-VAR(y, x, theta)
-- else if COMPOUND?(x) and COMPOUND?(y) then
--   return UNIFY(x.args, y.args, UNIFY(x.OP, y.OP, theta))
-- else if LIST?(x) and LIST?(y) then
--   return UNIFY(x.REST, y.REST, UNIFY(x.FIRST, y.FIRST, theta))
-- else return failure
--
-- function UNIFY-VAR(var, x, theta) returns a substitution
-- inputs: var, a variable
--         x, any expression
--         theta, the substitution built up so far
--
-- if {var/val} \in theta then return UNIFY(val, x, theta)
-- else if {x/val} \in theta then return UNIFY(var, val, theta)
-- else if OCCUR-CHECK?(var, x) then return failure *
-- else return add {var/x} to theta
--
-- * Prolog omits this

unify :: Term -> Term -> Maybe Sub
unify x y = unify' x y (Just (Sub Map.empty))

unify' :: Term -> Term -> Maybe Sub -> Maybe Sub
unify' _ _ Nothing = Nothing
unify' x y theta
  | x == y = theta
unify' (Variable x) y theta = unifyVar (Variable x) y theta
unify' x (Variable y) theta = unifyVar (Variable y) x theta
unify' (Function p xs) (Function q ys) theta
  | (p /= q) || (length xs /= length ys) = Nothing
  | otherwise = foldl (\t (x,y) -> unify' x y t) theta (zip xs ys)
unify' _ _ _ = Nothing

unifyVar :: Term -> Term -> Maybe Sub -> Maybe Sub
unifyVar _ _ Nothing = Nothing
unifyVar var x (Just (Sub m))
  | Map.member var m = case Map.lookup var m of
    (Just val) -> unify' val x (Just (Sub m))
  | Map.member x m = case Map.lookup x m of
    (Just val) -> unify' x val (Just (Sub m))
  | otherwise = Just (Sub (Map.insert var x m))

-- Artificial Intelligence a Modern Approach (3rd edition):
-- Figure 9.3, page 332 
-- Function FOL-BC-ASK(KB, goals, theta) returns a set of substitutions
-- inputs: KB, a knowledge base
--         goals, a list of conjuncts forming a query (theta already applied) 
--         theta, the current substitution, initially the empty substitution {}
-- local variables: answers, a set of substitutions, initially empty
--
-- if goals is empty then return {theta}
-- qDelta <- SUBST(theta, FIRST(goals))
-- for each sentence r in KB where STANDARDIZE-APART(r) = (p1 ^ ... ^ pn => q)
--     and thetaDelta <- UNIFY(q, qDelta) succeeds
--   new_goals <- [p1,...,pn|REST(goals)]
--   answers <- FOL-BC-ASK(KB, new_goals, COMPOSE(thetaDelta, theta)) U answers
-- return answers
