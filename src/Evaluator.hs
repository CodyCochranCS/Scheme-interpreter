{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Evaluator
    ( eval
    , extractSingleValue
    ) where

import qualified Data.Text as T
import qualified Data.IntMap.Strict as IM (lookup)
import Data.IORef (readIORef)
import Control.Monad.Except (throwError) --  catchError
import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ask)

import SchemeExpr


pairsToList :: Expr -> IO Expr
pairsToList (Pair ref) = do
  (a,b) <- readIORef ref
  a' <- pairsToList a
  b' <- pairsToList b
  return (a' :. b')
pairsToList x = return x

extractSingleValue :: Expr -> Eval Expr
extractSingleValue (x :. Null) = return x
extractSingleValue _ = lift $ throwError $ "Expected single value"

eval :: Expr -> Env -> Eval Expr
eval (Symbol (s, symbolid) :. args) env = do
  SpecialFormTable formTable <- ask
  case IM.lookup symbolid formTable of
    Just f -> f args env
    Nothing -> do
      f <- eval (Symbol (s,symbolid)) env >>= extractSingleValue
      eval (f :. args) env
eval (Lambda f :. args) env = do
  let eval_args Null = return Null
      eval_args (a :. as) = do
        val <- eval a env >>= extractSingleValue
        rest <- eval_args as
        return (val :. rest)
      eval_args _ = lift $ throwError "Function not called with list"
  args' <- eval_args args
  f args'
eval (fn@(_ :. _) :. args) env = do
  f <- eval fn env >>= extractSingleValue
  eval (f :. args) env
eval (a :. _) _ = lift $ throwError $ T.pack $ "Not a function: " ++ show a
eval Null _ = lift $ throwError $ "Call to null function"
eval (Symbol (s,symbolid)) env = do
  envs <- liftIO $ (traverse readIORef) env
  case msum $ fmap (IM.lookup symbolid) envs of
    Just val -> return (val :. Null)
    Nothing  -> lift $ throwError $ T.pack $ "Unbound variable: " ++ show s
eval p@(Pair _) env = do
  expr <- liftIO $ pairsToList p
  eval expr env
eval selfevaluating _ = return (selfevaluating :. Null)




