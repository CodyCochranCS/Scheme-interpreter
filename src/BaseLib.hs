{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module BaseLib
    ( createBaseEnv
    , for_each
    , write
    , newline
    ) where

import qualified Data.Text as T
import qualified Data.IntMap.Strict as IM (IntMap, fromList)
import Data.IORef (newIORef, readIORef, modifyIORef)
import Control.Monad.Except (throwError) --  catchError
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.RWS.Strict (runRWST)

import Evaluator
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
        car' _ = lift $ throwError "Called car on non-pair"
car _ = throwWrongArgs

cdr :: Expr -> Eval Expr
cdr (expr :. Null) = cdr' expr
  where cdr' :: Expr -> Eval Expr
        cdr' (_ :. b) = return (b :. Null)
        cdr' (Pair ref) = do
          (_,b) <- liftIO $ readIORef ref
          return (b :. Null)
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

createBaseEnv :: SymbolTable -> IO Env
createBaseEnv symbolTable = do 
  let sread :: Expr -> Eval Expr
      sread Null = sread' T.empty
        where sread' :: T.Text -> Eval Expr
              sread' inputBuffer = do
                input <- liftIO getLine
                let accumulatedInput = T.append inputBuffer $ T.pack (input ++ "\n")
                result <- liftIO $ runPromptResultT $ runRWST p_expr symbolTable accumulatedInput
                case result of
                  Success (expr, remaining, ()) -> return (expr :. Null)
                  Failure msg -> do
                    liftIO $ putStrLn $ "Parser Error: " ++ msg
                    return Null
                  Incomplete -> sread' accumulatedInput
      sread _ = throwWrongArgs

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
    ,("read", Lambda sread)
    ]
  fmap return $ newIORef $ IM.fromList $ environment

