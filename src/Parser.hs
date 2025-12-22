{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser
    ( p_exprs
    , p_expr
    , assign_symbol
    , PromptResult(..)
    , PromptResultT(..)
    ) where

-- import Control.Monad.Trans.State.Strict (StateT, get, put)
-- import Control.Monad.Reader (ReaderT)
import Control.Monad.RWS.Strict (RWST, runRWST, put, get, ask)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef
import Control.Applicative
import Data.Bifunctor

import SchemeExpr

data PromptResult a = Success a
                    | Failure String
                    | Incomplete
  deriving (Show, Eq, Functor)

newtype PromptResultT m a = PromptResultT {
    runPromptResultT :: m (PromptResult a)
}

instance Applicative PromptResult where
  pure = Success
  (<*>) f a = case (f, a) of
    (Success f', Success a') -> Success (f' a')
    (Failure e, _)           -> Failure e
    (_, Failure e)           -> Failure e
    (_, _)                   -> Incomplete

instance Alternative PromptResult where
  empty = Failure "Alternative.empty failed."
  p1 <|> p2 = case p1 of
    Success x  -> Success x
    Incomplete -> Incomplete
    Failure _  -> p2

instance MonadPlus PromptResult where
  mzero = Failure "mzero"

instance Monad PromptResult where
  return = Success
  m >>= k = case m of
    Success a  -> k a
    Failure e  -> Failure e
    Incomplete -> Incomplete

instance Monad m => Monad (PromptResultT m) where
    return = PromptResultT . return . Success
    m >>= f = PromptResultT $ do
        result <- runPromptResultT m
        case result of
            Success a    -> runPromptResultT (f a)
            Failure err  -> return $ Failure err
            Incomplete   -> return Incomplete

instance Monad m => Functor (PromptResultT m) where
    fmap f m = m >>= return . f

instance Monad m => Applicative (PromptResultT m) where
    pure = return
    a <*> b = do
        a' <- a
        b' <- b
        return (a' b')

instance Monad m => Alternative (PromptResultT m) where
    empty = PromptResultT $ return $ Failure "Alternative.empty failed."
    
    p1 <|> p2 = PromptResultT $ do
        res1 <- runPromptResultT p1
        case res1 of
            Success x  -> return $ Success x
            Incomplete -> return Incomplete
            Failure _  -> runPromptResultT p2

instance Monad m => MonadPlus (PromptResultT m) where
    mzero = empty
    mplus = (<|>)

instance MonadTrans PromptResultT where
    lift m = PromptResultT $ m >>= return . Success

instance MonadIO (PromptResultT IO) where
    liftIO = lift . liftIO

type Parser = RWST SymbolTable () T.Text (PromptResultT IO)

assign_symbol :: SymbolTable -> (T.Text, a) -> IO (Int, a)
assign_symbol symbolTable (str,lambda) = do
  (table,counter) <- readIORef symbolTable
  let newTable = HM.insert str counter table
  writeIORef symbolTable (newTable, counter+1)
  return (counter, lambda)

throwParseError :: String -> Parser a
throwParseError err = lift $ PromptResultT $ return $ Failure err

throwIncomplete :: Parser a
throwIncomplete = lift $ PromptResultT $ return Incomplete

p_eof :: Parser ()
p_eof = do
  t <- get
  if T.null t then
    return ()
  else
    throwParseError $ "EOF: Expected end of input, but still had: " ++ T.unpack t

p_not_eof :: Parser ()
p_not_eof = do
  t <- get
  if T.null t then
    throwParseError "End of input reached"
  else
    return ()

oneOf :: T.Text -> Parser Char
oneOf elements = do
  t <- get
  case (T.uncons t) of
    Nothing     -> throwIncomplete
    Just (x,xs) -> if (T.elem x elements) then
                       put xs >> return x
                     else
                       throwParseError $ "Parse error. Expected one of: " ++ T.unpack elements

softOneOf :: T.Text -> Parser Char
softOneOf elements = do
  t <- get
  case T.uncons t of
    Nothing -> throwParseError "EOF"
    Just (x,xs) -> 
      if T.elem x elements then
        put xs >> return x
      else
        throwParseError "No match"

noneOf :: T.Text -> Parser Char
noneOf elements = do
  t <- get
  case (T.uncons t) of
    Nothing     -> throwIncomplete
    Just (x,xs) -> if (T.elem x elements) then
                     throwParseError $ "Parse error. Bad input: " ++ [x]
                   else
                     put xs >> return x

maybeOneOf :: T.Text -> Parser (Maybe Char)
maybeOneOf elements = do
  t <- get
  case (T.uncons t) of
    Nothing     -> throwIncomplete
    Just (x,xs) -> if (T.elem x elements) then
                     put xs >> return (Just x)
                   else do
                     put t >> return Nothing

maybeParse :: Parser a -> Parser (Maybe a)
maybeParse p = (p >>= return . Just) <|> return Nothing

p_any :: Parser Char
p_any = do
  t <- get
  case (T.uncons t) of
    Nothing -> throwIncomplete
    Just (x,xs) -> put xs >> return x
    

allOf :: T.Text -> Parser String
allOf = traverse (\c -> oneOf $ T.pack [c]) . T.unpack

p_digit :: Parser Char
p_digit = oneOf $ T.pack ['0'..'9']

p_hex_digit :: Parser Char
p_hex_digit = p_digit <|> (oneOf $ T.pack ['a'..'f'])

p_hex_scalar_value :: Parser String
p_hex_scalar_value = some p_hex_digit

p_inline_hex_escape :: Parser Integer
p_inline_hex_escape = do
    allOf "\\x"
    hex_string <- p_hex_scalar_value
    let convert_digit = fromIntegral . fromEnum
    let digitval x = convert_digit x - convert_digit '0'
    return $ foldr (\x acc -> 16*acc + digitval x) 0 hex_string



p_explicit_sign :: Parser Char
p_explicit_sign = oneOf "+-"

p_letter :: Parser Char
p_letter = oneOf $ T.pack $ ['a'..'z'] ++ ['A'..'Z']

p_special_initial :: Parser Char
p_special_initial = oneOf $ T.pack $ "!$%&*/:<=>?^_~"

p_special_subsequent :: Parser Char
p_special_subsequent = p_explicit_sign <|> (oneOf $ T.pack ".@")

p_initial :: Parser Char
p_initial = p_explicit_sign <|> p_letter <|> p_special_initial

p_subsequent :: Parser Char
p_subsequent = p_initial <|> p_digit <|> p_special_subsequent

p_whitespace :: Parser Char
p_whitespace = softOneOf " \r\n\t"

p_vertical_line :: Parser Char
p_vertical_line = oneOf "|"

-- Expr parsers:

p_string :: Parser Expr
p_string = do
  oneOf "\""
  str <- many $ do {
                oneOf "\\";
                c <- p_any;
                return $ case c of
                    'a' -> '\a'
                    'b' -> '\b'
                    't' -> '\t'
                    'n' -> '\n'
                    'r' -> '\r'
                    otherwise -> c
            }
            <|> (noneOf "\"") 
  oneOf "\""
  return $ String $ T.pack str

p_integer :: Parser Expr
p_integer = do
  neg <- maybeOneOf "-+"
  ds <- some p_digit
  return $ Integer $ read ds * case neg of
    Just '-'  -> -1
    otherwise ->  1

p_symbol :: Parser Expr
p_symbol = do
  t <- p_initial
  ts <- many p_subsequent
  let symbol = T.pack (t:ts)
  ref <- ask
  (symbolTable, counter) <- liftIO $ readIORef ref
  case HM.lookup symbol symbolTable of
    Just n  -> return $ Symbol (symbol, n)
    Nothing -> do
      liftIO $ writeIORef ref ((HM.insert symbol counter symbolTable), counter+1)
      return $ Symbol (symbol, counter)

p_list :: Parser Expr
p_list = do
  many p_whitespace
  oneOf "("
  exprs <- many p_expr
  optionalDot <- do
    maybeParse $ oneOf "."
    maybeRest <- maybeParse p_expr
    return maybeRest
  oneOf ")"
  many p_whitespace
  return $ foldr (:.) (maybe Null id optionalDot) exprs

p_quote :: Parser Expr
p_quote = do
  oneOf "'"
  expr <- p_expr
  ref <- ask
  (symbolTable, counter) <- liftIO $ readIORef ref
  case HM.lookup "quote" symbolTable of
    Just n  -> return $ (Symbol ("quote", n) :. expr :. Null)
    Nothing -> throwParseError "Unable to find \"quote\" symbol"

p_expr :: Parser Expr
p_expr = do
  many p_whitespace
  result <- p_list <|> p_integer <|> p_string <|> p_quote <|> p_symbol
  many p_whitespace
  return result

p_exprs :: Parser [Expr]
p_exprs = do
  expr <- many $ do
    p_not_eof
    p_expr
  p_eof
  return expr