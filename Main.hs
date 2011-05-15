import Control.Monad                 
import Data.List
import System.Environment
import System.IO

import Datalog

-- Generic exception handling; just print the error
handler :: IOError -> IO ()
handler e = putStrLn $ show e

-- Attempts to read a list of a rules from the file at 'path'
readKb :: String -> IO [Rule]
readKb path = do contents <- readFile path
                 return $ readRules contents

-- Prompts the user for input and attempts to parse it as a term
readQuery :: IO Sentence
readQuery = do putStr "> "
               hFlush stdout
               input <- getLine
               return (read input :: Sentence)

-- Read-eval-print loop
readEvalPrint :: [Rule] -> IO ()
readEvalPrint kb = do query <- readQuery
                      let results = ask kb query
                      mapM_ print $ nub results

-- Reads a kb then loops forever answering queries               
main' :: String -> IO ()
main' path = do kb <- readKb path
                mapM_ print kb
                forever $ catch (readEvalPrint kb) handler

-- Main entrypoint
main :: IO ()
main = do args <- getArgs
          let usage = "<exec> [kb.path]"
          case args of 
               [path] -> catch (main' path) handler
               _      -> error usage
