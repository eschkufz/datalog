module Datalog
( Term(..)
, Sentence(..)
, Literal(..)
, Rule(..)
, ask
, rules
, sentence
) where

import qualified Data.Map as Map    
import qualified Data.Set as Set   

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token

-- Type hierarchy:

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

-- Parsing:

datalog :: LanguageDef st
datalog = emptyDef 
          { commentStart  = "/*"
          , commentEnd    = "*/"
          , commentLine   = "//"
          , identStart    = alphaNum <|> char '_' 
          , identLetter   = alphaNum <|> char '_'
          , caseSensitive = True 
          }

tp :: TokenParser st
tp = makeTokenParser datalog

tConstant :: Parser Term
tConstant = do c <- lexeme tp $ identifier tp
               return (Constant c)

tVariable :: Parser Term
tVariable = do _ <- char '?'
               v <- lexeme tp $ identifier tp
               return (Variable v)

tFunction :: Parser Term
tFunction = do _  <- symbol tp "("
               p  <- lexeme tp $ identifier tp
               xs <- manyTill term $ symbol tp ")"
               return (Function p xs)

term :: Parser Term
term = try tVariable <|> try tFunction <|> tConstant

tProposition :: Parser Sentence
tProposition = do p <- lexeme tp $ identifier tp
                  return (Proposition p)

tRelation :: Parser Sentence
tRelation = do _  <- symbol tp "("
               p  <- lexeme tp $ identifier tp
               xs <- manyTill term $ symbol tp ")"
               return (Relation p xs)

sentence :: Parser Sentence
sentence = try tProposition <|> tRelation

lSentence :: Parser Literal
lSentence = do s <- sentence
               return (Sentence s)

lNot :: Parser Literal
lNot = do _ <- symbol tp "("
          _ <- symbol tp "not"
          l <- literal
          _ <- symbol tp ")"
          return (Not l)

lDistinct :: Parser Literal
lDistinct = do _  <- symbol tp "("
               _  <- symbol tp "distinct"
               t1 <- term
               t2 <- term
               _  <- symbol tp ")"
               return (Distinct t1 t2)

literal :: Parser Literal
literal = try lNot <|> try lDistinct <|> lSentence

rSimple :: Parser Rule
rSimple = do h <- sentence
             return (Rule h [])

rComplex :: Parser Rule
rComplex = do _ <- symbol tp "("
              _ <- symbol tp "<="
              h <- sentence
              b <- manyTill literal $ symbol tp ")"
              return (Rule h b)

rule :: Parser Rule
rule = try rSimple <|> rComplex

rules :: Parser [Rule]
rules = do _  <- whiteSpace tp
           rs <- many rule
           return rs

-- Show:

showMany :: (Show a) => [a] -> String
showMany = unwords . map show

instance Show Term where
    show (Constant c)    = c
    show (Variable v)    = "?" ++ v
    show (Function p []) = "( " ++ p ++ " )"
    show (Function p xs) = "( " ++ p ++ " " ++ showMany xs ++ " )" 

instance Show Sentence where
    show (Proposition p) = p
    show (Relation p []) = "( " ++ p ++ " )"
    show (Relation p xs) = "( " ++ p ++ " " ++ showMany xs ++ " )"

instance Show Literal where
    show (Sentence s)     = show s
    show (Not l)          = "( not " ++ show l ++ " )"
    show (Distinct t1 t2) = "( distinct " ++ show t1 ++ " " ++ show t2 ++ " )"

instance Show Rule where
    show (Rule h []) = show h 
    show (Rule h b)  = "( <= " ++ show h ++ " " ++ showMany b ++ " )" 

-- Substitution:
-- Artificial Intelligence a Modern Approach (3rd edition)
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

-- Unification: 
-- Artificial Intelligence a Modern Approach (3rd edition)
-- Figure 9.1, page 328

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

-- Composition:
-- Artificial Intelligence a Modern Approach (3rd edition)
-- (Text), page ???

compose :: Sub -> Sub -> Sub
compose t1 t2 = Map.union t1 t2

-- Standardizing apart:
-- Artificial Intelligence a Modern Approach (3rd edition)
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

-- Backward Checking:
-- Artificial Intelligence a Modern Approach (3rd edition):
-- Figure 9.3, page 332 

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

-- Ask interface:
-- Binds each resulting substitutions to the original query

ask :: [Rule] -> Sentence -> [Sentence]
ask kb query = map subst' results
    where subst'  = flip subst query
          results = Set.toList $ folBcAsk kb [(Sentence query)] Map.empty
