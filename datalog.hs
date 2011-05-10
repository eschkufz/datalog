import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec

data Term = Constant String
          | Variable String
          | Function String [Term]
          deriving (Eq, Ord)

data Rule = Rule Term [Term]
          deriving (Eq, Ord)

ident :: Parser String
ident = many1 alphaNum

term :: Parser Term
term = do try (char '?')
          v <- ident
          return (Variable v)
   <|> do try (char '(')
          spaces
          p <- ident
          spaces
          xs <- term `sepEndBy` spaces
          char ')'
          return (Function p xs)
   <|> do c <- ident
          return (Constant c)

rule :: Parser Rule
rule = do try (char '(' >> skipMany space >> string "<=")
          spaces
          h <- term
          spaces
          b <- term `sepEndBy` spaces
          char ')'
          return (Rule h b)
   <|> do h <- term
          return (Rule h [])

showTerm :: String -> Term -> String
showTerm s t = s ++ " " ++ show t
 
instance Show Term where
  show (Constant c)    = c
  show (Variable v)    = "?" ++ v
  show (Function p xs) = "( " ++ p ++ (foldl showTerm "" xs) ++ " )" 
     
instance Show Rule where
  show (Rule h b) = "( <= " ++ show h ++ (foldl showTerm "" b) ++ " )" 

-- Artificial Intelligence a Modern Approach (3rd edition):
-- (Text), page ???
--
-- These follow from the defintion of substitution

type Sub = Map.Map Term Term

subst :: Sub -> Term -> Term
subst _ (Constant c) = (Constant c)
subst theta (Variable v) = case Map.lookup (Variable v) theta of
  (Just val) -> subst theta val
  Nothing    -> (Variable v)
subst theta (Function p xs) = (Function p (map (subst theta) xs))

-- Artificial Intelligence a Modern Approach (3rd edition):
-- (Text), page ???
--
-- COMPOSE(t1, t2) is the substitution whose effect is identical to the 
-- effect of applying each substitution in turn.  That is,
-- 
-- SUBST(COMPOSE(t1, t2), p) = SUBST(t2, SUBST(t1, p))

compose :: Sub -> Sub -> Sub
compose t1 t2 = Map.union t1 t2

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
-- * This is omitted by prolog

unify :: Term -> Term -> Maybe Sub
unify x y = unify' x y (Just Map.empty)

unify' :: Term -> Term -> Maybe Sub -> Maybe Sub
unify' _ _ Nothing = Nothing
unify' x y theta
  | x == y = theta
unify' (Variable x) y theta = unifyVar (Variable x) y theta
unify' x (Variable y) theta = unifyVar (Variable y) x theta
unify' (Function p xs) (Function q ys) theta
  | (p /= q) || (length xs /= length ys) = Nothing
  | otherwise = foldl unifyArg theta (zip xs ys)
    where unifyArg theta (x,y) = unify' x y theta
unify' _ _ _ = Nothing

unifyVar :: Term -> Term -> Maybe Sub -> Maybe Sub
unifyVar _ _ Nothing = Nothing
unifyVar var x (Just theta)
  | Map.member var theta = case Map.lookup var theta of
    (Just val) -> unify' val x (Just theta)
  | Map.member x theta = case Map.lookup x theta of
    (Just val) -> unify' x val (Just theta)
  | otherwise = Just (Map.insert var x theta)

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

folBcAsk :: [Rule] -> Term -> [Term]
folBcAsk kb query = fmap (\theta -> subst theta query) (Set.toList results)
  where results = folBcAsk' kb [query] Map.empty

folBcAsk' :: [Rule] -> [Term] -> Sub -> Set.Set Sub
folBcAsk' _ [] theta = Set.singleton theta
folBcAsk' kb goals theta = foldl tryRule Set.empty kb
  where qDelta = subst theta (head goals)
        tryRule answers (Rule q ps) = case unify q qDelta of
          (Just td) -> Set.union answers (folBcAsk' kb newGoals newTheta)
                       where newGoals = ps ++ (tail goals)
                             newTheta = compose td theta
          Nothing -> answers
    
-- Driver code
-- Parses a knowledge base and enters an infinite ask loop

readKb :: String -> IO [Rule]
readKb path = do contents <- readFile path
                 case parse (rule `sepEndBy` spaces) "" contents of
                      (Left pe)  -> error (show pe)
                      (Right ts) -> return ts

main = do args <- getArgs
          if null args 
             then error "No knowledge base specified!"
             else do kb <- readKb (head args)
                     mapM_ print kb
                     forever $ do
                               putStr "> "
                               hFlush stdout
                               input <- getLine
                               case parse term "" input of
                                    (Left pe) -> putStrLn (show pe)
                                    (Right q) -> mapM_ print (folBcAsk kb q)
