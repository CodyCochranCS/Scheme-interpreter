module SchemeExpr
    ( Expr(..)
    , Env
    , Eval
    ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)
import Control.Monad.Cont (ContT)
import Data.IORef (IORef)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM (HashMap)
import Data.Complex
import Data.Ratio

type Env = [IORef (HM.HashMap T.Text Expr)]

type Eval a = ContT [Expr] (ExceptT T.Text (ReaderT Env IO)) a
-- ContT [Expr] (ExceptT T.Text (ReaderT Env IO)) a
-- (a -> Env -> IO (Either Text [Expr]))  -> Env -> IO (Either Text [Expr])

data Expr = Integer Integer
          | Fraction Rational
          | Complex (Complex Expr)
          | Float Double
          | Bool Bool
          | Char Char
          | String T.Text
          | Symbol T.Text
          | List [Expr]
          | Pair (Expr, Expr)
          | Quote Expr
          | Lambda ([Expr] -> Eval [Expr])

instance Show Expr where
  show (Integer x) = show x
  show (Fraction x) = mconcat [show (numerator x), "/", show (denominator x)]
  show (Complex (r:+i)) = mconcat [show r, "+", show i]
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Char c) = "#\\" ++ [c]
  show (String s) = show s
  show (Symbol s) = T.unpack s
  show (Quote e) = "'" ++ show e
  show (Lambda _) = "<Lambda_function>"
  show (Pair (a,b)) = mconcat ["(", show a, " . ", show b, ")"]
  show (List xs) = "(" ++ go xs
                     where go [] = ")"
                           go [x] = mconcat [show x, ")"]
                           go (x:xs) = mconcat [show x, " ", go xs]
