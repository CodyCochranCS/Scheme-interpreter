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

plus (Integer x :. Integer y :. Null) = return [Integer (x+y)]
plus _ = throwWrongArgs

minus (Integer x :. Integer y :. Null) = return [Integer (x-y)]
minus _ = throwWrongArgs

times (Integer x :. Integer y :. Null) = return [Integer (x*y)]
times _ = throwWrongArgs

equal (Integer x :. Integer y :. Null) = return [Bool (x == y)]
equal _ = throwWrongArgs

lessorequal (Integer x :. Integer y :. Null) = return [Bool (x <= y)]
lessorequal _ = throwWrongArgs
{-
cons (a :. b :. Null) = case b of
  Quote x -> return [Quote (a :. x)]
  _ -> return [Quote (a :. b)]
cons _ = throwWrongArgs

car (Quote (a :. _) :. Null) = return [a]
car _ = throwWrongArgs

cdr (Quote (_ :. b) :. Null) = return [b]
cdr _ = throwWrongArgs
-}
newline Null = (liftIO $ putStrLn "") >> return []
newline _ = throwWrongArgs
{-
apply [Lambda f, Quote (List args)] = f args
apply _ = throwWrongArgs

eval' [Quote expr, Environment e] = eval expr e
eval' _ = throwWrongArgs
-}
{-
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
-}

createBaseEnv :: IO Env
createBaseEnv = fmap return $ newIORef $ HM.fromList $
  [("+", Lambda plus)
  ,("-", Lambda minus)
  ,("*", Lambda times)
  ,("=", Lambda equal)
  ,("<=", Lambda lessorequal)
  -- ,("cons", Lambda cons)
  -- ,("car", Lambda car)
  -- ,("cdr", Lambda cdr)
  -- ,("display", Lambda display)
  ,("newline", Lambda newline)
  -- ,("values", Lambda return)
  -- ,("apply", Lambda apply)
  -- ,("eval", Lambda eval')
  ]

eval :: Expr -> Env -> Eval [Expr]
eval (Symbol s :. args) env = case s of
  "define" -> case args of
    (Symbol identifier :. expr :. Null) -> do
      case env of
        (env_ref:_) -> do
          value <- eval expr env >>= extractSingleValue
          liftIO $ modifyIORef env_ref (HM.insert identifier value)
          return []
        _ -> lift $ throwError $ "No environment exists"
    _ -> lift $ throwError $ "Incorrect syntax for \"define\""
  "set!" -> case args of
    (Symbol identifier :. expr :. Null) -> do
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
    (test :. truebody :. falsebody :. Null) -> do
      condition <- eval test env >>= extractSingleValue
      case condition of
        Bool False -> eval falsebody env
        _          -> eval truebody env
    _ -> lift $ throwError "Syntax error with \"if\""
  "quote" -> case args of
    (x :. Null) -> return $ [Quote x]
    _ -> lift $ throwError "Syntax error with \"quote\""
  "lambda" -> case args of
    (params :. exprs) -> do
      let make_new_env = liftIO . newIORef . HM.fromList
          get_symbol (Symbol s) = s
          get_symbol _ = ""
          make_new_frame = case params of
            ps@(_ :. _) -> let go Null Null = Just []
                               go (p1 :. ps) (a1 :. as) = (:) <$> Just (get_symbol p1, a1) <*> go ps as
                               go _ _ = Nothing
                           in go ps
            Symbol p -> (\args -> Just [(p, Quote args)])
            _ -> const Nothing
      return $ return $ Lambda $ \args -> do
        case make_new_frame args of
          Nothing -> lift $ throwError "Error: wrong arguments with lambda"
          Just new_frame -> do
            local_env <- make_new_env new_frame
            let new_env = local_env:env
                go Null = return []
                go (final_expr :. Null) = eval final_expr new_env
                go (e :. es) = eval e new_env >> go es
            go exprs
    _ -> lift $ throwError "Syntax error with \"lambda\""
  "call/cc" -> case args of
    (fn :. Null) -> callCC $ \k -> do
      let pairstolist Null = return []
          pairstolist (a :. b) = (a:) <$> pairstolist b
          pairstolist _ = lift $ throwError "Wrong number of arguments for continuation"
      (`eval` env) $ (fn :. Lambda (\pairs -> pairstolist pairs >>= k) :. Null)
    _ -> lift $ throwError "Wrong number of arguments for \"call/cc\""
  "get-environment" -> case args of
    Null -> return $ [Environment env]
    _ -> lift $ throwError "Incorrect number of arguments for get-environment"
  _ -> do
    function <- eval (Symbol s) env >>= extractSingleValue
    case function of
      (Lambda f) -> do
        let eval_args Null = return Null
            eval_args p@(a :. as) = do
              val <- eval a env >>= extractSingleValue
              rest <- eval_args as
              return (val :. rest)
            eval_args _ = lift $ throwError "Error: Function not called with list"
        args' <- eval_args args
        f args'
      _ -> lift $ throwError "Error: Not a function"
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



