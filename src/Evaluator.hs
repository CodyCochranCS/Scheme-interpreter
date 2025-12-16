{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}


module Evaluator
    ( eval
    , createBaseEnv
    ) where

import qualified Data.Text as T
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM (HashMap, insert, lookup, fromList, adjust)
import Data.IORef (newIORef, readIORef, modifyIORef)
import Data.Function (fix)
import Control.Monad.Reader (ReaderT,runReaderT, ask, local, liftIO, lift)
import Control.Monad.Cont (evalContT, callCC)
import Control.Monad.Except (throwError, catchError, runExceptT)
import Control.Monad (msum, foldM)


import SchemeExpr -- for the Expr types

createBaseEnv :: IO Env
createBaseEnv = fmap return $ newIORef $ HM.fromList $
  [("+", Lambda (\[Integer x, Integer y] -> return [Integer (x+y)]))
  ,("-", Lambda (\[Integer x, Integer y] -> return [Integer (x-y)]))
  ,("*", Lambda (\[Integer x, Integer y] -> return [Integer (x*y)]))
  ,("=", Lambda (\[Integer x, Integer y] -> return [Bool (x == y)]))
  ,("<=", Lambda (\[Integer x, Integer y] -> return [Bool (x <= y)]))
  ,("cons", Lambda (\[a,b] -> return $ case b of
                                       Quote (List bs) -> [Quote (List (a:bs))]
                                       otherwise -> [Pair (a,b)]))
  ,("car", Lambda (\[p] -> return $ case p of
                                    Pair (a,_) -> [a]
                                    Quote (List (a:_)) -> [a]))
  ,("cdr", Lambda (\[p] -> return $ case p of
                                    Pair (_,b) -> [b]
                                    Quote (List (_:bs)) -> [Quote (List bs)]))
  ,("display", Lambda (\[String s] -> (liftIO $ putStrLn $ T.unpack s) >> return []))
  ,("values", Lambda (\values -> return values))
  ,("apply", Lambda (\[fn, Quote (List args)] -> eval $ List (fn:args)))
  ,("eval", Lambda (\[input] -> case input of
                       Quote x   -> eval x
                       otherwise -> return [input]))
  ]

eval :: Expr -> Eval [Expr]
eval (List (Symbol s:args)) = case s of
  "define" -> case args of
    [Symbol identifier,expr] -> do
      (env_ref:_) <- ask
      value <- eval expr >>= extractSingleValue
      liftIO $ modifyIORef env_ref (HM.insert identifier value)
      return []
    otherwise -> lift $ throwError $ "Incorrect syntax for \"define\""
  "set!" -> case args of
    [Symbol identifier,expr] -> do
      value <- eval expr >>= extractSingleValue
      let update_var [] = lift $ throwError $ T.pack $ "Unbound variable: " ++ show identifier
          update_var (env_ref:parent_envs) = do
            env <- liftIO $ readIORef env_ref
            case HM.lookup identifier env of
              Just _ -> liftIO $ modifyIORef env_ref (HM.adjust (const value) identifier)
              Nothing -> update_var parent_envs
      ask >>= update_var
      return []
    otherwise -> lift $ throwError "Syntax error with \"set!\""
  "if" -> case args of
    [test, truebody, falsebody] -> do
      condition <- eval test >>= extractSingleValue
      case condition of
        Bool False -> eval falsebody
        otherwise  -> eval truebody
    otherwise -> lift $ throwError "Syntax error with \"if\""
  "quote" -> case args of
    [x] -> return $ [Quote x]
    otherwise -> lift $ throwError "Syntax error with \"quote\""
  "lambda" -> case args of
    (params:exprs) -> do
      let make_new_env = liftIO . newIORef . HM.fromList
      make_new_frame <- case params of
        List ps  -> return (\args -> make_new_env $ zip (map (\(Symbol s) -> s) ps) args)
        Symbol p -> return (\args -> make_new_env $ [(p, Quote (List args))])
        otherwise -> lift $ throwError "Syntax error with lambda parameter list"
      env <- ask
      return $ return $ Lambda $ \args -> do
        new_env_ref <- make_new_frame args
        local (const $ new_env_ref:env) $ go exprs
          where go [] = return []
                go [final_expr] = eval final_expr 
                go (e:es) = eval e >> go es
    otherwise -> lift $ throwError "Syntax error with \"lambda\""
  "call/cc" -> case args of
    [fn] -> callCC $ \k -> eval $ List $ fn : [Lambda k]
    otherwise -> lift $ throwError "Wrong number of arguments for \"call/cc\""
  otherwise -> do
    function <- eval (Symbol s) >>= extractSingleValue
    eval $ List (function:args)
eval (List []) = lift $ throwError $ "Cannot evaluate empty function"
eval (List function) = do
  result <- traverse (\e -> eval e >>= extractSingleValue) function
  case result of
    (Lambda f:args) -> f args
    otherwise  -> lift $ throwError $ "Not a function"
eval x@(Quote _) = return [x]
eval (Symbol s) = do
  envs <- ask >>= liftIO . traverse readIORef
  case msum $ fmap (HM.lookup s) envs of
    Just val -> return [val]
    Nothing  -> lift $ throwError $ T.pack $ "Unbound variable: " ++ show s
eval selfevaluating = return [selfevaluating]

extractSingleValue :: [Expr] -> Eval Expr
extractSingleValue [x] = return x
extractSingleValue xs = lift $ throwError $ "Expected single value"



