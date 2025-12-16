{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.State.Strict (evalStateT)
import Control.Monad.Cont (evalContT)
import Control.Monad.Except (runExceptT)
import Control.Monad (forever)
import Data.Foldable (for_)
import qualified Data.Text as T
import System.IO
import SchemeExpr
import Evaluator
import Parser


repl :: IO ()
repl = do
  baseEnv <- createBaseEnv

  putStrLn "Start of REPL"
  forever $ do
    putStr "=> "
    hFlush stdout
    input <- getLine
    case evalStateT p_expr (T.pack input) of
      Nothing -> putStrLn "Error: Failed to parse"
      Just expr -> do
        result <- (`runReaderT` baseEnv) $ runExceptT $ evalContT $ eval expr
        case result of
          Left err -> putStrLn $ "Error: " ++ T.unpack err
          Right values -> for_ values (putStrLn . show)


main :: IO ()
main = repl
