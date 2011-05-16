import qualified Control.Exception as Exc
import qualified Control.Monad as Monad             
import qualified Data.List as List
import qualified System.Environment as Env
import qualified System.IO as IO

import Datalog

-- Generic exception handling; just print the error
handler :: Exc.SomeException -> IO ()
handler e = putStrLn $ show e

-- Attempts to read a list of a rules from the file at 'path'
readKb :: String -> IO [Rule]
readKb path = do contents <- readFile path
                 return $ readRules contents

-- Prompts the user for input and attempts to parse it as a term
readQuery :: IO Sentence
readQuery = do putStr "> "
               IO.hFlush IO.stdout
               input <- getLine
               return (read input :: Sentence)

-- Read-eval-print loop
readEvalPrint :: [Rule] -> IO ()
readEvalPrint kb = do query <- readQuery
                      let results = ask kb query
                      mapM_ print $ List.nub results

-- Reads a kb then loops forever answering queries               
main' :: String -> IO ()
main' path = do kb <- readKb path
                mapM_ print kb
                Monad.forever $ Exc.catch (readEvalPrint kb) handler

-- Main entrypoint
main :: IO ()
main = do args <- Env.getArgs
          let usage = "<exec> [kb.path]"
          case args of 
               [path] -> Exc.catch (main' path) handler
               _      -> error usage
