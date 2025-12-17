{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Evaluator
    ( eval
    , createBaseEnv
    ) where

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM (insert, lookup, fromList, adjust)
import Data.IORef (newIORef, readIORef, modifyIORef)
import Control.Monad.Cont (callCC)
import Control.Monad.Except (throwError) --  catchError
import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)

import SchemeExpr

throwWrongArgs :: Eval [Expr]
throwWrongArgs = lift $ throwError $ "Incorrect arguments used in function call"

createBaseEnv :: IO Env
createBaseEnv = fmap return $ newIORef $ HM.fromList $
  [("+", Lambda (\args -> case args of
                   [Integer x, Integer y] -> return [Integer (x+y)]
                   _ -> throwWrongArgs))
  ,("-", Lambda (\args -> case args of
                   [Integer x, Integer y] -> return [Integer (x-y)]
                   _ -> throwWrongArgs))
  ,("*", Lambda (\args -> case args of
                   [Integer x, Integer y] -> return [Integer (x*y)]
                   _ -> throwWrongArgs))
  ,("=", Lambda (\args -> case args of
                   [Integer x, Integer y] -> return [Bool (x == y)]
                   _ -> throwWrongArgs))
  ,("<=", Lambda (\args -> case args of
                    [Integer x, Integer y] -> return [Bool (x <= y)]
                    _ -> throwWrongArgs))
  ,("cons", Lambda (\args -> case args of
                      [a,b] -> return $ case b of
                                       Quote (List bs) -> [Quote (List (a:bs))]
                                       _ -> [Pair (a,b)]
                      _ -> throwWrongArgs))
  ,("car", Lambda (\args -> case args of
                      [Pair (a,_)] -> return [a]
                      [Quote (List (a:_))] -> return [a]
                      _ -> throwWrongArgs))
  ,("cdr", Lambda (\args -> case args of
                      [Pair (_,b)] -> return [b]
                      [Quote (List (_:bs))] -> return [Quote (List bs)]
                      _ -> throwWrongArgs))
  ,("display", Lambda (\args -> case args of
                         [String s] -> (liftIO $ putStrLn $ T.unpack s) >> return []
                         _ -> throwWrongArgs))
  ,("values", Lambda return)
  ,("apply", Lambda (\args -> case args of
                        [Lambda f, Quote (List args)] -> f args
                        _ -> throwWrongArgs))
  ,("eval", Lambda (\args -> case args of
                      [Quote expr, Environment e] -> eval expr e
                      _ -> throwWrongArgs))
  ]

eval :: Expr -> Env -> Eval [Expr]
eval (List (Symbol s:args)) env = case s of
  "define" -> case args of
    [Symbol identifier,expr] -> do
      case env of
        (env_ref:_) -> do
          value <- eval expr env >>= extractSingleValue
          liftIO $ modifyIORef env_ref (HM.insert identifier value)
          return []
        _ -> lift $ throwError $ "No environment exists"
    _ -> lift $ throwError $ "Incorrect syntax for \"define\""
  "set!" -> case args of
    [Symbol identifier,expr] -> do
      value <- eval expr env >>= extractSingleValue
      let update_var [] = lift $ throwError $ T.pack $ "Unbound variable: " ++ show identifier
          update_var (env_ref:parent_envs) = do
            env <- liftIO $ readIORef env_ref
            case HM.lookup identifier env of
              Just _ -> liftIO $ modifyIORef env_ref (HM.adjust (const value) identifier)
              Nothing -> update_var parent_envs
      update_var env
      return []
    _ -> lift $ throwError "Syntax error with \"set!\""
  "if" -> case args of
    [test, truebody, falsebody] -> do
      condition <- eval test env >>= extractSingleValue
      case condition of
        Bool False -> eval falsebody env
        _          -> eval truebody env
    _ -> lift $ throwError "Syntax error with \"if\""
  "quote" -> case args of
    [x] -> return $ [Quote x]
    _ -> lift $ throwError "Syntax error with \"quote\""
  "lambda" -> case args of
    (params:exprs) -> do
      let make_new_env = liftIO . newIORef . HM.fromList
      make_new_frame <- case params of
        List ps  -> return (\args -> make_new_env $ zip (map (\x -> case x of
                                                                      (Symbol s) -> s
                                                                      _ -> "")
                                                              ps)
                                                        args)
        Symbol p -> return (\args -> make_new_env $ [(p, Quote (List args))])
        _ -> lift $ throwError "Syntax error with lambda parameter list"
      return $ return $ Lambda $ \args -> do
        new_frame <- make_new_frame args
        let new_env = new_frame:env
            go [] = return []
            go [final_expr] = eval final_expr new_env
            go (e:es) = eval e new_env >> go es
        go exprs
    _ -> lift $ throwError "Syntax error with \"lambda\""
  "call/cc" -> case args of
    [fn] -> callCC $ \k -> (`eval` env) $ List $ fn : [Lambda k]
    _ -> lift $ throwError "Wrong number of arguments for \"call/cc\""
  "get-environment" -> case args of
    [] -> return $ [Environment env]
    _ -> lift $ throwError $ "Incorrect number of arguments for get-environment"
  _ -> do
    function <- eval (Symbol s) env >>= extractSingleValue
    (`eval` env) $ List (function:args)
eval (List []) _ = lift $ throwError $ "Cannot evaluate empty function"
eval (List function) env = do
  result <- traverse (\e -> eval e env >>= extractSingleValue) function
  case result of
    (Lambda f:args) -> f args
    _  -> lift $ throwError $ "Not a function"
eval x@(Quote _) _ = return [x]
eval (Symbol s) env = do
  envs <- liftIO $ (traverse readIORef) env
  case msum $ fmap (HM.lookup s) envs of
    Just val -> return [val]
    Nothing  -> lift $ throwError $ T.pack $ "Unbound variable: " ++ show s
eval selfevaluating _ = return [selfevaluating]

extractSingleValue :: [Expr] -> Eval Expr
extractSingleValue [x] = return x
extractSingleValue _ = lift $ throwError $ "Expected single value"



