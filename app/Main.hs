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
          Right [] -> return ()
          Right values -> for_ values (putStrLn . show)


main :: IO ()
main = repl

  {-
  baseEnv <- createBaseEnv
  (`runReaderT` baseEnv) $ do {
    repl $ List [Symbol "define", Symbol "x", Integer 3];
    repl $ List [Symbol "define", Symbol "y", Integer 5];
    repl $ List [Symbol "+", Symbol "x", Symbol "y"];
    repl $ List [Symbol "if", List [Symbol "=", Symbol "x", Integer 42], String "x was 42", String "x was not 42"];
    repl $ List [Symbol "set!", Symbol "x", Integer 42];
    repl $ List [Symbol "if", List [Symbol "=", Symbol "x", Integer 42], String "x was 42", String "x was not 42"];
    repl $ List [Symbol "define", Symbol "p", List [Symbol "cons", Integer 3, Char 'A']];
    repl $ Symbol "p";
    repl $ List [Symbol "car", Symbol "p"];
    repl $ List [Symbol "cdr", Symbol "p"];
    repl $ List [Symbol "set!", Symbol "p", List [Symbol "cons", Integer 3, List [Symbol "cons", Integer 4, Quote (List [])]]];
    repl $ Symbol "p";
    repl $ List [Symbol "car", Symbol "p"];
    repl $ List [Symbol "cdr", Symbol "p"];
    repl $ Quote $ List [];
    
    repl $ List [Symbol "define", Symbol "sqr", List [Symbol "lambda", List [Symbol "w"], List [Symbol "*", Symbol "w", Symbol "w"]]];
    repl $ List [Symbol "sqr", Integer 9];
    repl $ List [Symbol "define", Symbol "fact"
                ,List [Symbol "lambda", List [Symbol "x"]
                      ,List [Symbol "if", List [Symbol "<=", Symbol "x", Integer 0]
                            ,Integer 1
                            ,List [Symbol "*", Symbol "x", List [Symbol "fact", List [Symbol "-", Symbol "x", Integer 1]]]]]];
    repl $ List [Symbol "fact", Integer 5];
    
    repl $ List [Symbol "define", Symbol "test", List [Symbol "lambda", List [], Symbol "delayed"]];
    repl $ List [Symbol "define", Symbol "delayed", Integer 1234];
    repl $ List [Symbol "test"];
    repl $ List [Symbol "display", String "This is just a test of the print function"];
    
        repl $ List [Symbol "define", Symbol "arglisttest",
                                  List [Symbol "lambda", Symbol "args", List [Symbol "display", String "arglist"], Symbol "args"]];
    repl $ List [Symbol "arglisttest", Integer 1, Integer 2, Integer 3];
    
    repl $ List [Symbol "define", Symbol "result1", 
                   List [Symbol "call/cc", List [Symbol "lambda", List [Symbol "k"], 
                                                  Integer 1]]];
    repl $ Symbol "result1";
    
    repl $ List [Symbol "define", Symbol "result2", 
                   List [Symbol "call/cc", List [Symbol "lambda", List [Symbol "k"], 
                                                  List [Symbol "k", Integer 1]]]];
    repl $ Symbol "result2";
    
    repl $ List [Symbol "define", Symbol "result3", 
                   List [Symbol "call/cc", List [Symbol "lambda", List [Symbol "k"], 
                                                  List [Symbol "+", List [Symbol "k", Integer 2], Integer 1]]]];
    repl $ Symbol "result3";
    
    repl $ List [Symbol "define", Symbol "result", 
                   List [Symbol "call/cc", List [Symbol "lambda", List [Symbol "k"], 
                                                  List [Symbol "k", Integer 42], 
                                                  Integer 99]]];
    repl $ Symbol "result";
    
    repl $ List [Symbol "define", Symbol "test", List [Symbol "+",
            List [Symbol "call/cc", List [Symbol "lambda", List [Symbol "k"], List [Symbol "k", Integer 1]]],
            List [Symbol "call/cc", List [Symbol "lambda", List [Symbol "k"], List [Symbol "k", Integer 2]]]]];
    repl $ Symbol "test";
    
    repl $ List [Symbol "values", String "okay", Integer 5, Char 'Z'];
    
    repl $ List [Symbol "apply", Symbol "+", Quote (List [Integer 1, Integer 2])];
    
    repl $ List [Symbol "eval", Quote (List [Symbol "+", Integer 1, Integer 2])];

    repl $ List [Symbol "define", Symbol "tailcalltest",
      List [Symbol "lambda", List [Symbol "n", Symbol "acc"],
        List [Symbol "if", List [Symbol "=", Symbol "n", Integer 0],
          Symbol "acc",
          List [Symbol "tailcalltest",
            List [Symbol "-", Symbol "n", Integer 1],
            List [Symbol "+", Symbol "acc", Integer 1]]]]];
    
    repl $ List [Symbol "tailcalltest", Integer 10, Integer 0];
  }
  -}


