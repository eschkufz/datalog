import qualified Control.Monad as Monad             
import qualified Data.List as List
import qualified System.Environment as Env
import qualified System.IO as IO
import qualified System.IO.Error as Err
import qualified Text.ParserCombinators.Parsec as Par

import Datalog

-- Exception Handling: print user errors; punt on everything else
handler :: IOError -> IO ()
handler e 
    | Err.isUserError e = print e
    | otherwise         = ioError e

-- Attempts to read the knowledge base at 'path'
readKb :: String -> IO [Rule]
readKb path = do res <- Par.parseFromFile rules path
                 case res of 
                      (Left e)   -> ioError $ userError (show e)
                      (Right rs) -> return rs

-- Attempts to parse user input as a sentence
readQuery :: IO Sentence
readQuery = do putStr "> "
               IO.hFlush IO.stdout
               input <- getLine
               let res = Par.parse sentence "" input 
               case res of
                    (Left e)  -> ioError $ userError (show e)
                    (Right s) -> return s

-- Read-eval-print loop
readEvalPrint :: [Rule] -> IO ()
readEvalPrint kb = do q <- readQuery
                      let results = ask kb q
                      mapM_ print $ List.nub results

-- Reads a kb then loops forever answering queries               
main' :: String -> IO ()
main' path = do kb <- readKb path
                mapM_ print kb
                Monad.forever $ catch (readEvalPrint kb) handler

-- Main entrypoint
main :: IO ()
main = do args <- Env.getArgs
          let usage = "<exec> [kb.path]"
          case args of 
               [path] -> catch (main' path) handler
               _      -> error usage
