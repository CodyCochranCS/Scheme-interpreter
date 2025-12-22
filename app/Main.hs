{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Control.Monad.Reader (runReaderT)
import Control.Monad.Cont (evalContT)
import Control.Monad.Except (runExceptT)
import Data.Foldable (for_)
import Data.Function (fix)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Control.Monad.RWS.Strict (runRWST)
import Data.IORef (newIORef)
import System.IO (hFlush, stdout)

import SchemeExpr
import Evaluator
import Parser
import BaseLib
import SpecialForms

repl :: SpecialFormTable -> SpecialFormTable
     -> Env -> SymbolTable -> T.Text -> IO ()
repl expandForms evalForms env symbolTable = fix $ \loop inputBuffer -> do
  putStr $ case T.null inputBuffer of
    True  -> "=> "
    False -> "   "
  hFlush stdout
  input <- getLine
  let accumulatedInput = T.append inputBuffer $ T.pack (input ++ "\n")
  result <- runPromptResultT $ runRWST p_exprs symbolTable accumulatedInput
  case result of
    Success (exprs, _, _) -> do
      for_ exprs $ \expr -> do
        let evaluated = do
              vals <- eval expr env
              let writeln args = write args >> newline Null
              for_each (Lambda writeln :. vals :. Null)
        result <- runExceptT $ (`runReaderT` evalForms) $ evalContT $ evaluated
        case result of
          Left err -> putStrLn $ "Error: " ++ T.unpack err
          Right _  -> return ()
      hFlush stdout
      loop T.empty
    Failure msg -> do
      putStrLn $ "Parser Error: " ++ msg
      loop T.empty
    Incomplete -> loop accumulatedInput

main :: IO ()
main = do
  symbolTable <- newIORef (HM.empty, 0)
  (expandForms, evalForms) <- createSpecialForms symbolTable
  baseEnv <- createBaseEnv symbolTable
  putStrLn "Start of REPL"
  repl expandForms evalForms baseEnv symbolTable T.empty
