module SchemeExpr
    ( Expr(..)
    , Env
    , SymbolTable
    , SpecialFormTable(..)
    , Eval
    ) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Cont (ContT)
import Control.Monad.Reader (ReaderT)
import Data.IORef (IORef)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM (HashMap)
import qualified Data.IntMap.Strict as IM
import Data.Complex
import Data.Ratio

-- type Env = [IORef (HM.HashMap T.Text Expr)]
type Env = [IORef (IM.IntMap Expr)]
type SymbolTable = IORef ((HM.HashMap T.Text Int), Int)
newtype SpecialFormTable = SpecialFormTable (IM.IntMap (Expr -> Env -> Eval Expr))

-- type Eval = ContT Expr (ExceptT T.Text IO)
type Eval = ContT Expr (ReaderT SpecialFormTable (ExceptT T.Text IO))
-- ContT [Expr] (ExceptT T.Text IO) a
-- (a -> IO (Either Text [Expr]))  -> IO (Either Text [Expr])

infixr 5 :.

data Expr = Integer Integer
          | Fraction Rational
          | Complex (Complex Expr)
          | Float Double
          | Bool Bool
          | Char Char
          | String T.Text
          | Symbol (T.Text, Int)
          | Expr :. Expr
          | Pair (IORef (Expr, Expr))
          | Null
          | Quote Expr
          | Lambda (Expr -> Eval Expr)
          | Environment Env

instance Show Expr where
  show (Integer x) = show x
  show (Fraction x) = mconcat [show (numerator x), "/", show (denominator x)]
  show (Complex (r:+i)) = mconcat [show r, "+", show i]
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Char c) = "#\\" ++ [c]
  show (String s) = show s
  show (Symbol (s,_)) = T.unpack s
  show (Quote e) = "'" ++ show e
  show (Lambda _) = "<Lambda_function>"
  show p@(_ :. _) = "(" ++ go p
                    where go (a :. Null) = mconcat [show a, ")"]
                          go (a :. b) = mconcat [show a, " ", go b]
                          go end = mconcat [". ", show end, ")"]
  show (Environment _) = "<Environment>"
  show Null = "()"
