{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Evaluator
    ( eval
    , createBaseEnv
    , for_each
    , write
    , newline
    , sread
    ) where

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM (insert, lookup, fromList, adjust)
import Data.IORef (newIORef, readIORef, modifyIORef)
import Control.Monad.Cont (callCC)
import Control.Monad.Except (throwError) --  catchError
import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Strict (evalStateT, runStateT)
import Parser

import SchemeExpr

throwWrongArgs :: Eval Expr
throwWrongArgs = lift $ throwError $ "Incorrect arguments used in function call"

plus (Integer x :. Integer y :. Null) = return (Integer (x+y) :. Null)
plus _ = throwWrongArgs

minus (Integer x :. Integer y :. Null) = return (Integer (x-y) :. Null)
minus _ = throwWrongArgs

times (Integer x :. Integer y :. Null) = return (Integer (x*y) :. Null)
times _ = throwWrongArgs

equal (Integer x :. Integer y :. Null) = return (Bool (x == y) :. Null)
equal _ = throwWrongArgs

lessorequal (Integer x :. Integer y :. Null) = return (Bool (x <= y) :. Null)
lessorequal _ = throwWrongArgs

cons (a :. b :. Null) = return ((a :. b) :. Null)
cons _ = throwWrongArgs

car :: Expr -> Eval Expr
car (expr :. Null) = car' expr
  where car' :: Expr -> Eval Expr
        car' (a :. _) = return (a :. Null)
        car' (Pair ref) = do
          (a,_) <- liftIO $ readIORef ref
          return (a :. Null)
        car' (Quote x) = case x of
          (_ :. _) -> car' x
          Pair _   -> car' x
          _ -> lift $ throwError "Called car on non-pair"
        car' _ = lift $ throwError "Called car on non-pair"
car _ = throwWrongArgs

cdr :: Expr -> Eval Expr
cdr (expr :. Null) = cdr' expr
  where cdr' :: Expr -> Eval Expr
        cdr' (_ :. b) = return (b :. Null)
        cdr' (Pair ref) = do
          (_,b) <- liftIO $ readIORef ref
          return (b :. Null)
        cdr' (Quote x) = case x of
          (_ :. _) -> cdr' x
          Pair _   -> cdr' x
          _ -> lift $ throwError "Called car on non-pair"
        cdr' _ = lift $ throwError "Called car on non-pair"
cdr _ = throwWrongArgs

newline Null = (liftIO $ putStrLn "") >> return Null
newline _ = throwWrongArgs

apply (Lambda f :. Quote args)= f args
apply _ = throwWrongArgs

eval' (Quote expr :. Environment e :. Null) = eval expr e
eval' _ = throwWrongArgs

-- Note: change this later to be variadic
for_each (Lambda f :. lst :. Null) = do
  case lst of
    Null -> return Null
    (x :. xs) -> do
      f (x :. Null)
      for_each (Lambda f :. xs :. Null)
    _ -> lift $ throwError "Unexpected end of list in for-each"
for_each _ = throwWrongArgs

-- requires environment atm
sread :: Expr -> Eval Expr
sread Null = sread' T.empty
  where sread' :: T.Text -> Eval Expr
        sread' inputBuffer = do
          input <- liftIO getLine
          let accumulatedInput = T.append inputBuffer $ T.pack (input ++ "\n")
          result <- liftIO $ runPromptResultT $ runStateT p_expr accumulatedInput
          case result of
            Success (expr, remaining) -> case expr of
                (_ :. _) -> return (Quote expr :. Null)
                _ -> return (expr :. Null)
            Failure msg -> do
              liftIO $ putStrLn $ "Parser Error: " ++ msg
              return Null
            Incomplete -> sread' accumulatedInput
-- sread _ = throwWrongArgs
sread _ = lift $ throwError $ "wrong args in read"

write :: Expr -> Eval Expr
write (expr :. Null) = write' expr
  where writerest :: Expr -> Eval Expr
        writerest rest = case rest of
          Null -> liftIO $ putStr ")" >> return Null
          (a :. b) -> do
            liftIO $ putStr " "
            write' a
            writerest b
          Pair ref -> do
            (a,b) <- liftIO $ readIORef ref
            liftIO $ putStr " "
            write' a
            writerest b
          _ -> do
            liftIO $ putStr " . "
            write' rest
            liftIO $ putStr ")"
            return Null
        write' :: Expr -> Eval Expr
        write' (a :. b) = do
          liftIO $ putStr "("
          write' a
          writerest b
        write' (Pair ref) = do
          (a,b) <- liftIO $ readIORef ref
          liftIO $ putStr "("
          write' a
          writerest b
          -- add other mutable types here
        write' (Quote x) = do
          liftIO $ putStr "'"
          write' x
          return Null
        write' expr = do
          liftIO $ putStr $ show expr
          return Null
write _ = lift $ throwError $ "wrong args in write"

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
  ,("cons", Lambda cons)
  ,("car", Lambda car)
  ,("cdr", Lambda cdr)
  -- ,("display", Lambda display)
  ,("newline", Lambda newline)
  ,("values", Lambda return)
  ,("apply", Lambda apply)
  ,("eval", Lambda eval')
  ,("write", Lambda write)
  ,("read", Lambda sread)
  ]

listToPairs :: Expr -> IO Expr
listToPairs (a :. b) = do
  a' <- listToPairs a
  b' <- listToPairs b
  ref <- newIORef (a',b')
  return (Pair ref)
listToPairs x = return x

pairsToList :: Expr -> IO Expr
pairsToList (Pair ref) = do
  (a,b) <- readIORef ref
  a' <- pairsToList a
  b' <- pairsToList b
  return (a' :. b')
pairsToList x = return x

eval :: Expr -> Env -> Eval Expr
eval (Symbol s :. args) env = case s of
  "define" -> case args of
    (Symbol identifier :. expr :. Null) -> do
      case env of
        (env_ref:_) -> do
          value <- eval expr env >>= extractSingleValue
          liftIO $ modifyIORef env_ref (HM.insert identifier value)
          return Null
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
      return Null
    _ -> lift $ throwError "Syntax error with \"set!\""
  "if" -> case args of
    (test :. truebody :. falsebody :. Null) -> do
      condition <- eval test env >>= extractSingleValue
      case condition of
        Bool False -> eval falsebody env
        _          -> eval truebody env
    _ -> lift $ throwError "Syntax error with \"if\""
  "quote" -> case args of
    -- (x :. Null) -> return $ (Quote x :. Null)
    (x :. Null) -> return $ (x :. Null)
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
    _ -> lift $ throwError "Syntax error with \"lambda\""
  "call/cc" -> case args of
    (fn :. Null) -> callCC $ \k -> (`eval` env) $ (fn :. Lambda k :. Null)
    _ -> lift $ throwError "Wrong number of arguments for \"call/cc\""
  "get-environment" -> case args of
    Null -> return $ (Environment env :. Null)
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
eval (_ :. _) _ = lift $ throwError "Error: Not a function"
eval x@(Quote _) _ = return (x :. Null)
eval (Symbol s) env = do
  envs <- liftIO $ (traverse readIORef) env
  case msum $ fmap (HM.lookup s) envs of
    Just val -> return (val :. Null)
    Nothing  -> lift $ throwError $ T.pack $ "Unbound variable: " ++ show s
eval selfevaluating _ = return (selfevaluating :. Null)

extractSingleValue :: Expr -> Eval Expr
extractSingleValue (x :. Null) = return x
extractSingleValue _ = lift $ throwError $ "Expected single value"



