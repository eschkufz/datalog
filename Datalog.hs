module Datalog
( Term(..)
, Sentence(..)
, Literal(..)
, Rule(..)
, readRules
, ask
) where

import qualified Data.Map as Map    
import qualified Data.Set as Set   
import Text.ParserCombinators.Parsec

-- Datalog type hierarchy

data Term = Constant String
          | Variable String
          | Function String [Term]
    deriving (Eq, Ord)

data Sentence = Proposition String
              | Relation String [Term]
    deriving (Eq, Ord)

data Literal = Sentence Sentence
             | Not Literal
             | Distinct Term Term
    deriving (Eq, Ord)

data Rule = Rule Sentence [Literal]
    deriving (Eq, Ord)

-- From string methods

ident :: Parser String
ident = many1 alphaNum

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

sentence :: Parser Sentence
sentence = do _  <- try (char '(')
              p  <- spaces >> ident
              xs <- spaces >> term `sepEndBy` spaces
              _  <- char ')'
              return (Relation p xs)
       <|> do p <- ident
              return (Proposition p)

literal :: Parser Literal
literal = do _ <- try (char '(' >> skipMany space >> string "not")
             l <- spaces >> literal 
             _ <- spaces >> char ')'
             return (Not l)
      <|> do _  <- try (char '(' >> skipMany space >> string "distinct")
             t1 <- spaces >> term
             t2 <- spaces >> term
             _  <- spaces >> char ')'
             return (Distinct t1 t2)
      <|> do s <- sentence
             return (Sentence s)

rule :: Parser Rule
rule = do _ <- try (char '(' >> skipMany space >> string "<=")
          h <- spaces >> sentence
          b <- spaces >> literal `sepEndBy` spaces
          _ <- char ')'
          return (Rule h b)
   <|> do h <- sentence
          return (Rule h [])

instance Read Term where
    readsPrec _ s = case parse term "" s of
                         (Left pe)   -> error $ show pe
                         (Right res) -> [(res, "")]

instance Read Sentence where
    readsPrec _ s = case parse sentence "" s of
                         (Left pe)   -> error $ show pe
                         (Right res) -> [(res, "")]

instance Read Literal where
    readsPrec _ s = case parse literal "" s of
                         (Left pe)   -> error $ show pe
                         (Right res) -> [(res, "")]

instance Read Rule where
    readsPrec _ s = case parse rule "" s of 
                         (Left pe)   -> error $ show pe
                         (Right res) -> [(res, "")]

readRules :: String -> [Rule]
readRules s = case parse (rule `sepEndBy` spaces) "" s of
                   (Left pe)  -> error (show pe)
                   (Right rs) -> rs

-- To string methods

showMany :: (Show a) => [a] -> String
showMany = unwords . map show

instance Show Term where
    show (Constant c)    = c
    show (Variable v)    = "?" ++ v
    show (Function p xs) = "( " ++ p ++ " " ++ showMany xs ++ " )" 

instance Show Sentence where
    show (Proposition p) = p
    show (Relation p xs) = "( " ++ p ++ " " ++ showMany xs ++ " )"

instance Show Literal where
    show (Sentence s)     = show s
    show (Not l)          = "( not " ++ show l ++ " )"
    show (Distinct t1 t2) = "( distinct " ++ show t1 ++ " " ++ show t2 ++ " )"

instance Show Rule where
    show (Rule h []) = show h 
    show (Rule h b)  = "( <= " ++ show h ++ " " ++ showMany b ++ " )" 

-- Substitution 
-- 
-- Artificial Intelligence a Modern Approach (3rd edition):
-- (Text), page ???

type Sub = Map.Map Term Term

class Subst a where
    subst :: Sub -> a -> a

instance Subst Term where
    subst _ (Constant c) = (Constant c)
    subst theta (Variable v) = case Map.lookup (Variable v) theta of
        (Just val) -> subst theta val
        Nothing    -> (Variable v)
    subst theta (Function p xs) = (Function p (map (subst theta) xs))

instance Subst Sentence where
    subst _ (Proposition p)     = (Proposition p)
    subst theta (Relation p xs) = (Relation p (map (subst theta) xs))

instance Subst Literal where
    subst theta (Sentence s)     = (Sentence (subst theta s))
    subst theta (Not l)          = (Not (subst theta l))
    subst theta (Distinct t1 t2) = (Distinct (subst theta t1) (subst theta t2))

-- Unification 
-- 
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

unify :: Sentence -> Sentence -> Maybe Sub
unify (Proposition p) (Proposition q) 
    | p /= q    = Nothing
    | otherwise = (Just Map.empty)
unify (Relation p xs) (Relation q ys)
    | p /= q                 = Nothing
    | length xs /= length ys = Nothing
    | otherwise              = foldl unify'' (Just Map.empty) pairs
          where unify'' = flip $ uncurry unify'
                pairs   = zip xs ys
unify _ _ = Nothing

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

-- Composition
-- 
-- Artificial Intelligence a Modern Approach (3rd edition):
-- (Text), page ???
--
-- COMPOSE(t1, t2) is the substitution whose effect is identical to the 
-- effect of applying each substitution in turn.  That is,
-- 
-- SUBST(COMPOSE(t1, t2), p) = SUBST(t2, SUBST(t1, p))

compose :: Sub -> Sub -> Sub
compose t1 t2 = Map.union t1 t2

-- Standardizing apart
-- 
-- Artificial Intelligence a Modern Approach (3rd edition):
-- (Text), page ???
     
class Standardize a where
    standardize :: a -> a

instance Standardize Term where
    standardize (Constant c)    = (Constant c)
    standardize (Variable v)    = (Variable ("_" ++ v))
    standardize (Function p xs) = (Function p (map standardize xs))

instance Standardize Sentence where
    standardize (Proposition p) = (Proposition p)
    standardize (Relation p xs) = (Relation p (map standardize xs))

instance Standardize Literal where
    standardize (Sentence s)     = (Sentence (standardize s))
    standardize (Not l)          = (Not (standardize l))
    standardize (Distinct t1 t2) = (Distinct (standardize t1) (standardize t2)) 

instance Standardize Rule where
    standardize (Rule h b) = (Rule (standardize h) (map standardize b))

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

folBcAsk :: [Rule] -> [Literal] -> Sub -> Set.Set Sub
folBcAsk _ [] theta = Set.singleton theta
folBcAsk kb ((Sentence s):ls) theta = foldl tryRule Set.empty kb'
   where kb'    = map standardize kb
         qDelta = subst theta s
         tryRule answers (Rule q ps) = case unify q qDelta of
             (Just td) -> Set.union answers $ folBcAsk kb' newGoals newTheta
                 where newGoals = ps ++ ls
                       newTheta = compose td theta
             Nothing   -> answers
folBcAsk kb ((Not l):ls) theta
    | Set.null $ folBcAsk kb [(subst theta l)] theta = folBcAsk kb ls theta
    | otherwise                                      = Set.empty
folBcAsk kb ((Distinct t1 t2):ls) theta
    | (subst theta t1) /= (subst theta t2) = folBcAsk kb ls theta
    | otherwise                            = Set.empty

-- A simple interface for querying a knowledge base
-- Binds each resulting substitutions to the original query
ask :: [Rule] -> Sentence -> [Sentence]
ask kb query = map subst' results
    where subst'  = flip subst query
          results = Set.toList $ folBcAsk kb [(Sentence query)] Map.empty
