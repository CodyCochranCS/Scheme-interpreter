{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( p_expr
    ) where

import Control.Monad.Trans.State.Strict
import qualified Data.Text as T
import Control.Applicative
import Data.Bifunctor

import SchemeExpr

type Parser = StateT T.Text Maybe



oneOf :: T.Text -> Parser Char
oneOf elements = StateT $ \t ->
  case (T.uncons t) of
    Nothing     -> Nothing
    Just (x,xs) -> if (T.elem x elements)
                     then Just (x,xs)
                     else Nothing

noneOf :: T.Text -> Parser Char
noneOf elements = StateT $ \t ->
  case (T.uncons t) of
    Nothing     -> Nothing
    Just (x,xs) -> if (T.elem x elements)
                     then Nothing
                     else Just (x,xs)

maybeOneOf :: T.Text -> Parser (Maybe Char)
maybeOneOf elements = StateT $ \t ->
  case (T.uncons t) of
    Nothing     -> Just (Nothing, t)
    Just (x,xs) -> if (T.elem x elements)
                   then Just (Just x,xs)
                   else Just (Nothing, t)

p_any :: Parser Char
p_any = StateT T.uncons
    

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
p_whitespace = oneOf " \n\t"

p_vertical_line :: Parser Char
p_vertical_line = oneOf "|"

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


p_identifier :: Parser T.Text
p_identifier = do
  many p_whitespace
  t <- p_initial
  ts <- many p_subsequent
  return $ T.pack (t:ts)

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
  oneOf "("
  exprs <- many $ do
    many p_whitespace
    p_expr
  oneOf ")"
  return $ List exprs

p_quote :: Parser Expr
p_quote = do
  oneOf "'"
  expr <- p_expr
  return $ Quote expr

p_expr :: Parser Expr
p_expr = p_list <|> p_integer <|> p_string <|> p_quote <|> p_symbol