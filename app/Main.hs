{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Trans.State.Strict (evalStateT, runStateT)
import Control.Monad.Cont (evalContT)
import Control.Monad.Except (runExceptT)
import Control.Monad (forever)
import Data.Foldable (for_)
import Data.Function (fix)
import qualified Data.Text as T
import System.IO
import SchemeExpr
import Evaluator
import Parser

repl :: Env -> T.Text -> IO ()
repl env = fix $ \loop inputBuffer -> do
  putStr $ case T.null inputBuffer of
    True  -> "=> "
    False -> "   "
  hFlush stdout
  input <- getLine
  let accumulatedInput = T.append inputBuffer $ T.pack (input ++ "\n")
  case runStateT p_exprs accumulatedInput of
    Success (exprs, remaining) -> do
      for_ exprs $ \expr -> do
        result <- runExceptT $ evalContT $ eval expr env
        case result of
          Left err     -> putStrLn $ "Error: " ++ T.unpack err
          Right values -> for_ values (putStrLn . show)
      loop T.empty
    Failure msg -> do
      putStrLn $ "Parser Error: " ++ msg
      loop T.empty
    Incomplete -> loop accumulatedInput

main :: IO ()
main = do
  putStrLn "Start of REPL"
  baseEnv <- createBaseEnv
  repl baseEnv T.empty
