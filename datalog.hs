import Control.Monad                 
import qualified Data.Map as Map    
import qualified Data.Set as Set   
import System.Environment
import System.IO
import System.IO.Error hiding (try)
import Text.ParserCombinators.Parsec

data Term = Constant String
          | Variable String
          | Function String [Term]
    deriving (Eq, Ord)

data Rule = Rule Term [Term]
    deriving (Eq, Ord)

term :: Parser Term
term = do _ <- try (char '?')
          v <- ident
          return (Variable v)
   <|> do _  <- try (char '(')
          p  <- spaces >> ident
          xs <- spaces >> term `sepEndBy` spaces
          _  <- char ')'
          return (Function p xs)
   <|> do c <- ident
          return (Constant c)
    where ident = many1 alphaNum

rule :: Parser Rule
rule = do _ <- try (char '(' >> skipMany space >> string "<=")
          h <- spaces >> term
          b <- spaces >> term `sepEndBy` spaces
          _ <- char ')'
          return (Rule h b)
   <|> do h <- term
          return (Rule h [])

instance Show Term where
    show (Constant c)    = c
    show (Variable v)    = "?" ++ v
    show (Function p xs) = "( " ++ p ++ " " ++ showTerms xs ++ " )" 
        where showTerms = unwords . map show
     
instance Show Rule where
    show (Rule h []) = show h 
    show (Rule h b)  = "( <= " ++ show h ++ " " ++ showTerms b ++ " )" 
        where showTerms = unwords . map show

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
subst theta (Function p xs) = (Function p (map subst' xs))
    where subst' = subst theta

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
    | p /= q                 = Nothing
    | length xs /= length ys = Nothing
    | otherwise              = foldl unify'' theta pairs
    where unify'' = flip $ uncurry unify'
          pairs   = zip xs ys
unify' _ _ _ = Nothing

unifyVar :: Term -> Term -> Maybe Sub -> Maybe Sub
unifyVar _ _ Nothing = Nothing
unifyVar var x (Just theta) 
    | Map.member var theta = unify' (theta Map.! var) x (Just theta)
    | Map.member x theta   = unify' x (theta Map.! x) (Just theta)
    | otherwise            = Just (Map.insert var x theta)
     
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

folBcAsk :: [Rule] -> Term -> Set.Set Sub
folBcAsk kb query = folBcAsk' kb [query] Map.empty

folBcAsk' :: [Rule] -> [Term] -> Sub -> Set.Set Sub
folBcAsk' _ [] theta = Set.singleton theta
folBcAsk' kb goals theta = foldl (tryRule qd) Set.empty kb
    where qd = subst theta (head goals)
          tryRule qDelta answers (Rule q ps) = case unify q qDelta of
              (Just td) -> Set.union answers (folBcAsk' kb newGoals newTheta)
                  where newGoals = ps ++ (tail goals)
                        newTheta = compose td theta
              Nothing   -> answers

-- Parses the contents of a file into a list of rules
readKb :: String -> IO [Rule]
readKb path = do contents <- readFile path
                 case parse (rule `sepEndBy` spaces) "" contents of
                      (Left pe)  -> ioError $ userError (show pe)
                      (Right rs) -> return rs

-- Parses user input from the command line into a term
readQuery :: IO Term
readQuery = do putStr "> "
               hFlush stdout
               input <- getLine
               case parse term "" input of
                    (Left pe) -> ioError $ userError (show pe)
                    (Right t) -> return t

-- Queries kb and returns a list of unique answers                    
ask :: [Rule] -> Term -> [Term]
ask kb query = Set.toList $ Set.map subst' results 
    where subst'  = flip subst query
          results = folBcAsk kb query

-- Generic exception handling: just print the error
handler :: IOError -> IO ()
handler e 
    | isUserError e = putStrLn $ ioeGetErrorString e
    | otherwise     = error "Unhandled exception!"

-- Reads a kb then loops forever answering queries               
main' :: String -> IO ()
main' path = do kb <- readKb path
                mapM_ print kb
                forever $ do query <- readQuery
                             mapM_ print $ ask kb query
                     `catch` handler
        `catch` handler          

-- main                              
main :: IO ()
main = do args <- getArgs
          let usage = "<exec> [kb.path]"
          case args of 
               [path] -> main' path
               _      -> error usage
