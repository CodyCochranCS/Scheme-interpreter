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
import qualified Data.IntMap.Strict as IM (IntMap)
import Data.Sequence (Seq)
import Data.Complex
import Data.Ratio

type Env = [IORef (IM.IntMap Expr)]
type SymbolTable = IORef ((HM.HashMap T.Text Int), Int)

type Eval = ContT Expr (ReaderT SpecialFormTable (ExceptT T.Text IO))
newtype SpecialFormTable = SpecialFormTable (IM.IntMap (Expr -> Env -> Eval Expr))

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
          | Lambda (Expr -> Eval Expr)
          | Environment Env
          | Pattern (Seq Expr)

instance Eq Expr where
  (Integer x) == (Integer y)   = x == y
  (Fraction x) == (Fraction y) = x == y
  (Complex x) == (Complex y)   = x == y
  (Float x) == (Float y)       = x == y
  (Bool x) == (Bool y)         = x == y
  (Char x) == (Char y)         = x == y
  (String x) == (String y)     = x == y
  (Symbol (_,x)) == (Symbol (_,y)) = x == y
  (a :. as) == (b :. bs)      = (a == b) && (as == bs)
  (Pair x) == (Pair y)        = x == y -- Note: checks for pointer equality, not deep comparison
  Null == Null                = True
  _ == _                      = False

instance Show Expr where
  show (Integer x) = show x
  show (Fraction x) = mconcat [show (numerator x), "/", show (denominator x)]
  show (Complex (r:+i)) = mconcat [show r, "+", show i]
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Char c) = "#\\" ++ [c]
  show (String s) = show s
  show (Symbol (s,_)) = T.unpack s
  show (Lambda _) = "<Lambda_function>"
  show p@(_ :. _) = "(" ++ go p
                    where go (a :. Null) = mconcat [show a, ")"]
                          go (a :. b) = mconcat [show a, " ", go b]
                          go end = mconcat [". ", show end, ")"]
  show (Environment _) = "<Environment>"
  show Null = "()"
  show (Pattern _) = "<Pattern>"
  