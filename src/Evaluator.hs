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

plus [Integer x, Integer y] = return [Integer (x+y)]
plus _ = throwWrongArgs

minus [Integer x, Integer y] = return [Integer (x-y)]
minus _ = throwWrongArgs

times [Integer x, Integer y] = return [Integer (x*y)]
times _ = throwWrongArgs

equal [Integer x, Integer y] = return [Bool (x == y)]
equal _ = throwWrongArgs

lessorequal [Integer x, Integer y] = return [Bool (x <= y)]
lessorequal _ = throwWrongArgs

cons [a,b] = return $ case b of
  Quote (List bs) -> [Quote (List (a:bs))]
  _ -> [Pair (a,b)]
cons _ = throwWrongArgs

car [Pair (a,_)] = return [a]
car [Quote (List (a:_))] = return [a]
car _ = throwWrongArgs

cdr [Pair (_,b)] = return [b]
cdr [Quote (List (_:bs))] = return [Quote (List bs)]
cdr _ = throwWrongArgs

newline [] = (liftIO $ putStrLn "") >> return []
newline _ = throwWrongArgs

apply [Lambda f, Quote (List args)] = f args
apply _ = throwWrongArgs

eval' [Quote expr, Environment e] = eval expr e
eval' _ = throwWrongArgs

display [Quote x] = (liftIO $ putStr $ show x) >> return []
display [x] = do
  case x of
    Pair (a,b) -> do
      liftIO $ putStr "("
      display [a]
      liftIO $ putStr " . "
      display [b]
      liftIO $ putStr ")"
    List [] -> liftIO $ putStr "()"
    List xs -> do
      liftIO $ putStr "("
      traverse (\x -> liftIO (putStr " ") >> display [x]) xs
      liftIO $ putStr ")"
    String s -> liftIO $ putStr $ T.unpack s
    x -> liftIO $ putStr $ show x
  return []
display _ = throwWrongArgs

createBaseEnv :: IO Env
createBaseEnv = fmap return $ newIORef $ HM.fromList $
  [("+", Lambda plus)
  ,("-", Lambda minus)
  ,("*", Lambda times)
  ,("=", Lambda equal)
  ,("<=", Lambda lessorequal)
  ,("cons", Lambda cons)
  ,("car", Lambda car)
  ,("cdr", Lambda cdr)
  ,("display", Lambda display)
  ,("newline", Lambda newline)
  ,("values", Lambda return)
  ,("apply", Lambda apply)
  ,("eval", Lambda eval')
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
          get_symbol (Symbol s) = s
          get_symbol _ = ""
      -- Note: zip doesn't verify same length between parameters and arguments
      -- Note: Change to a type to handle errors better
      make_new_frame <- case params of
        List ps  -> return (\args -> make_new_env $ zip (map get_symbol ps) args)
        Pair ps -> return (\args -> 
          let go (List ps) as = zip (map get_symbol ps) as
              go (Pair (p1,p2)) (a1:as) = (get_symbol p1, a1) : go p2 as
              go p lst = [(get_symbol p, List lst)]
          in make_new_env $ go (Pair ps) args)
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
eval p@(Pair (a,b)) env = case b of
  List as -> eval (List (a:as)) env
  _ -> lift $ throwError $ T.pack $ "Cannot evaluate pair: " ++ show p
eval selfevaluating _ = return [selfevaluating]

extractSingleValue :: [Expr] -> Eval Expr
extractSingleValue [x] = return x
extractSingleValue _ = lift $ throwError $ "Expected single value"



