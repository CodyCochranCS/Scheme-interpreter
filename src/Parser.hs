{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser
    ( p_exprs
    , PromptResult(..)
    ) where

import Control.Monad.Trans.State.Strict (StateT, get, put)
import Control.Monad.Trans (lift)
import Control.Monad (MonadPlus(..))
import qualified Data.Text as T
import Control.Applicative
import Data.Bifunctor

import SchemeExpr

data PromptResult a = Success a
                    | Failure String
                    | Incomplete
  deriving (Show, Eq, Functor)

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

type Parser = StateT T.Text PromptResult

p_eof :: Parser ()
p_eof = do
  t <- get
  if T.null t then
    return ()
  else
    lift $ Failure $ "EOF: Expected end of input, but still had: " ++ T.unpack t

p_not_eof :: Parser ()
p_not_eof = do
  t <- get
  if T.null t then
    lift $ Failure "End of input reached"
  else
    return ()

oneOf :: T.Text -> Parser Char
oneOf elements = do
  t <- get
  case (T.uncons t) of
    Nothing     -> lift Incomplete
    Just (x,xs) -> if (T.elem x elements) then
                       put xs >> return x
                     else
                       lift $ Failure $ "Parse error. Expected one of: " ++ T.unpack elements

softOneOf :: T.Text -> Parser Char
softOneOf elements = do
  t <- get
  case T.uncons t of
    Nothing -> lift $ Failure "EOF"
    Just (x,xs) -> 
      if T.elem x elements then
        put xs >> return x
      else
        lift $ Failure "No match"

noneOf :: T.Text -> Parser Char
noneOf elements = do
  t <- get
  case (T.uncons t) of
    Nothing     -> lift Incomplete
    Just (x,xs) -> if (T.elem x elements) then
                     lift $ Failure $ "Parse error. Bad input: " ++ [x]
                   else
                     put xs >> return x

maybeOneOf :: T.Text -> Parser (Maybe Char)
maybeOneOf elements = do
  t <- get
  case (T.uncons t) of
    Nothing     -> lift Incomplete
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
    Nothing -> lift Incomplete
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
  return $ Symbol $ T.pack (t:ts)

p_list :: Parser Expr
p_list = do
  many p_whitespace
  oneOf "("
  exprs <- many p_expr
  optionalDot <- do
    maybeParse $ oneOf "."
    maybeList <- maybeParse p_list
    return maybeList
  oneOf ")"
  many p_whitespace
  case optionalDot of
    Nothing          -> return $ List exprs
    Just (List rest) -> return $ List (exprs ++ rest)
    _ -> lift $ Failure "Parse error: No list after dot"

p_pair :: Parser Expr
p_pair = do
  oneOf "("
  expr1 <- p_expr
  oneOf "."
  expr2 <- p_expr
  oneOf ")"
  return $ Pair (expr1, expr2)

p_quote :: Parser Expr
p_quote = do
  oneOf "'"
  expr <- p_expr
  return $ Quote expr

p_expr :: Parser Expr
p_expr = do
  many p_whitespace
  result <- p_list <|> p_pair <|> p_integer <|> p_string <|> p_quote <|> p_symbol
  many p_whitespace
  return result

p_exprs :: Parser [Expr]
p_exprs = do
  expr <- many $ do
    p_not_eof
    p_expr
  p_eof
  return expr