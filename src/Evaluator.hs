{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Evaluator
    ( eval
    , createBaseEnv
    , createSpecialForms
    , for_each
    , write
    , newline
    -- , sread
    ) where

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM (insert, lookup, fromList, adjust)
import qualified Data.IntMap.Strict as IM
import Data.IORef (newIORef, readIORef, modifyIORef, writeIORef)
import Control.Monad.Cont (callCC)
import Control.Monad.Except (throwError) --  catchError
import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Strict (evalStateT, runStateT)
import Control.Monad.Reader (ask)
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

cons (a :. b :. Null) = do
  ref <- liftIO $ newIORef (a,b)
  return (Pair ref :. Null)
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

set_car :: Expr -> Eval Expr
set_car (expr :. val :. Null) = case expr of
  Pair ref -> do
    liftIO $ modifyIORef ref (\(a,b) -> (val,b))
    return Null
  (_ :. _) -> lift $ throwError "Error: Attempt to mutate immutable pair"
  _ -> lift $ throwError "Error: Attempt to mutate non-pair"
set_car _ = throwWrongArgs

set_cdr :: Expr -> Eval Expr
set_cdr (expr :. val :. Null) = case expr of
  Pair ref -> do
    liftIO $ modifyIORef ref (\(a,b) -> (a,val))
    return Null
  (_ :. _) -> lift $ throwError "Error: Attempt to mutate immutable pair"
  _ -> lift $ throwError "Error: Attempt to mutate non-pair"
set_cdr _ = throwWrongArgs

newline Null = (liftIO $ putStrLn "") >> return Null
newline _ = throwWrongArgs

apply (Lambda f :. args :. Null) = f args
apply _ = throwWrongArgs

eval' (expr :. Environment e :. Null) = eval expr e
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

{-
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
sread _ = throwWrongArgs
-}

recursive_write :: (Expr -> String) -> Expr -> Eval Expr
recursive_write style expr = case expr of
  (a :. b) -> do
    liftIO $ putStr "("
    recursive_write style a
    writerest b
  (Pair ref) -> do
    (a,b) <- liftIO $ readIORef ref
    liftIO $ putStr "("
    recursive_write style a
    writerest b
    -- add other mutable types here
  (Quote x) -> do
    liftIO $ putStr "'"
    recursive_write style x
    return Null
  _ -> do
    liftIO $ putStr $ style expr
    return Null
  where writerest :: Expr -> Eval Expr
        writerest rest = case rest of
          Null -> liftIO $ putStr ")" >> return Null
          (a :. b) -> do
            liftIO $ putStr " "
            recursive_write style a
            writerest b
          Pair ref -> do
            (a,b) <- liftIO $ readIORef ref
            liftIO $ putStr " "
            recursive_write style a
            writerest b
          _ -> do
            liftIO $ putStr " . "
            recursive_write style rest
            liftIO $ putStr ")"
            return Null

displayval (String s) = T.unpack s
displayval x = show x

write :: Expr -> Eval Expr
write (expr :. Null) = recursive_write show expr
write _ = throwWrongArgs

display :: Expr -> Eval Expr
display (expr :. Null) = recursive_write displayval expr
display _ = throwWrongArgs

assign_symbol :: SymbolTable -> (T.Text, a) -> IO (Int, a)
assign_symbol symbolTable (str,lambda) = do
  (table,counter) <- readIORef symbolTable
  let newTable = HM.insert str counter table
  writeIORef symbolTable (newTable, counter+1)
  return (counter, lambda)

createBaseEnv :: SymbolTable -> IO Env
createBaseEnv symbolTable = do 
  environment <- traverse (assign_symbol symbolTable)
    [("+", Lambda plus)
    ,("-", Lambda minus)
    ,("*", Lambda times)
    ,("=", Lambda equal)
    ,("<=", Lambda lessorequal)
    ,("cons", Lambda cons)
    ,("car", Lambda car)
    ,("cdr", Lambda cdr)
    ,("set-car!", Lambda set_car)
    ,("set-cdr!", Lambda set_cdr)
    ,("display", Lambda display)
    ,("newline", Lambda newline)
    ,("values", Lambda return)
    ,("apply", Lambda apply)
    ,("eval", Lambda eval')
    ,("write", Lambda write)
    -- ,("read", Lambda sread)
    ]
  fmap return $ newIORef $ IM.fromList $ environment

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
      get_symbol _ = -1 -- Todo: return an error if invalid symbol
      make_new_frame = case params of
        ps@(_ :. _) ->  let go Null Null = Just []
                            go (p1 :. ps) (a1 :. as) = Just ((get_symbol p1, a1):) <*> go ps as
                            go _ _ = Nothing
                        in go ps
        Symbol (_,p) -> (\args -> Just [(p, Quote args)])
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
eval (a :. _) _ = lift $ throwError $ T.pack $ "Not a function: " ++ show a
eval x@(Quote _) _ = return (x :. Null)
eval (Symbol (s,symbolid)) env = do
  envs <- liftIO $ (traverse readIORef) env
  case msum $ fmap (IM.lookup symbolid) envs of
    Just val -> return (val :. Null)
    Nothing  -> lift $ throwError $ T.pack $ "Unbound variable: " ++ show s
eval selfevaluating _ = return (selfevaluating :. Null)

extractSingleValue :: Expr -> Eval Expr
extractSingleValue (x :. Null) = return x
extractSingleValue _ = lift $ throwError $ "Expected single value"



