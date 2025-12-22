{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module SpecialForms
    ( createSpecialForms
    ) where

import qualified Data.Text as T
import qualified Data.IntMap.Strict as IM (fromList, adjust, lookup, insert)
import Data.IORef (newIORef, readIORef, modifyIORef)
import Control.Monad.Cont (callCC)
import Control.Monad.Except (throwError) --  catchError
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)

import Evaluator
import Parser
import SchemeExpr

listToPairs :: Expr -> IO Expr
listToPairs (a :. b) = do
  a' <- listToPairs a
  b' <- listToPairs b
  ref <- newIORef (a',b')
  return (Pair ref)
listToPairs x = return x

createSpecialForms :: SymbolTable -> IO (SpecialFormTable, SpecialFormTable)
createSpecialForms symbolTable = do 
  baseTable <- traverse (assign_symbol symbolTable)
    [("define", define)
    ,("set!", set_)
    ,("if", if_)
    ,("quote",quote)
    ,("lambda",lambda_)
    ,("call/cc", call_cc)
    ,("get-environment", get_environment)
    ]
  let base_intmap = SpecialFormTable (IM.fromList baseTable)
  return (base_intmap, base_intmap)

define :: Expr -> Env -> Eval Expr
define (Symbol (_, identifier) :. expr :. Null) env = do
  case env of
    (env_ref:_) -> do
      frame <- liftIO $ readIORef env_ref
      case IM.lookup identifier frame of
        Just _ -> lift $ throwError $ "Defining a variable that already exists"
        Nothing -> do
          value <- eval expr env >>= extractSingleValue
          mutable_value <- liftIO $ case value of
            (_ :. _) -> listToPairs value
            _ -> return value
          liftIO $ modifyIORef env_ref (IM.insert identifier mutable_value)
          return Null
    _ -> lift $ throwError $ "No environment exists"
define _ _ = lift $ throwError $ "Incorrect syntax for \"define\""

set_ :: Expr -> Env -> Eval Expr
set_ (Symbol (sym, identifier) :. expr :. Null) env = do
  value <- eval expr env >>= extractSingleValue
  mutable_value <- liftIO $ case value of
    (_ :. _) -> listToPairs value
    _ -> return value
  let update_var [] = lift $ throwError $ T.pack $ "Unbound variable: " ++ show sym
      update_var (env_ref:parent_envs) = do
        env <- liftIO $ readIORef env_ref
        case IM.lookup identifier env of
          Just _ -> liftIO $ modifyIORef env_ref (IM.adjust (const mutable_value) identifier)
          Nothing -> update_var parent_envs
  update_var env
  return Null
set_ _ _ = lift $ throwError "Syntax error with \"set!\""

if_ :: Expr -> Env -> Eval Expr
if_ (test :. truebody :. falsebody :. Null) env = do
  condition <- eval test env >>= extractSingleValue
  case condition of
    Bool False -> eval falsebody env
    _          -> eval truebody env
if_ _ _ = lift $ throwError "Syntax error with \"if\""

quote :: Expr -> Env -> Eval Expr
quote (x :. Null) _ = return $ (x :. Null)
quote _ _ = lift $ throwError "Syntax error with \"quote\""

lambda_ :: Expr -> Env -> Eval Expr
lambda_ (params :. exprs) env = do
  let make_new_env = liftIO . newIORef . IM.fromList
      get_symbol (Symbol (_,n)) = n
      get_symbol _ = -1 -- valid_symbols should mean this never gets reached
      valid_symbols Null = True
      valid_symbols (Symbol _) = True
      valid_symbols (Symbol _ :. ps) = valid_symbols ps
      valid_symbols _ = False
  if valid_symbols params then do
    let make_new_frame = case params of
          Symbol (_,p) -> (\args -> Just [(p, args)])
          ps -> let go Null Null = Just []
                    go (p1 :. ps) (a1 :. as) = Just ((get_symbol p1, a1):) <*> go ps as
                    go rest (a1 :. as) = Just [(get_symbol rest, (a1 :. as))]
                    go _ _ = Nothing
                in go ps
          _ -> const Nothing
        f = Lambda $ \args -> case make_new_frame args of
              Nothing -> lift $ throwError "Error: wrong arguments with lambda"
              Just new_frame -> do
                local_env <- make_new_env new_frame
                let new_env = local_env:env
                    go Null = return Null
                    go (final_expr :. Null) = eval final_expr new_env
                    go (e :. es) = eval e new_env >> go es
                go exprs
    return (f :. Null)
  else
    lift $ throwError "Non-symbol parameters in lambda definition"
lambda_ _ _ = lift $ throwError "Syntax error with \"lambda\""

call_cc :: Expr -> Env -> Eval Expr
call_cc (function :. Null) env = do
  f <- eval function env >>= extractSingleValue
  case f of
    (Lambda _) -> callCC $ \k -> (`eval` env) $ (f :. Lambda k :. Null)
    _ -> lift $ throwError $ T.pack $ "No function passed to Call/CC-- " ++ (show f)
call_cc _ _ = lift $ throwError "Wrong number of arguments for \"call/cc\""

get_environment :: Expr -> Env -> Eval Expr
get_environment Null env = return $ (Environment env :. Null)
get_environment _ _ = lift $ throwError "Incorrect number of arguments for get-environment"
