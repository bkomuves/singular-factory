
-- | Polynomial expressions (used for parsing)

{-# LANGUAGE BangPatterns, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Math.Singular.Factory.Expr where

--------------------------------------------------------------------------------

import Data.Char
import Data.List

import Control.Applicative
import Control.Monad

import Data.Text.Lazy ( Text )
import qualified Data.Text.Lazy      as T
import qualified Data.Text.Lazy.Read as T

import Math.Singular.Factory.Internal.DList as DList

--------------------------------------------------------------------------------
-- * Types

data Sign 
  = Plus 
  | Minus 
  deriving (Eq,Ord,Show)

negateIfMinus :: Num a => Sign -> a -> a
negateIfMinus Plus  = id
negateIfMinus Minus = negate

--------------------------------------------------------------------------------

-- | Monomials
newtype Monom var 
  = Monom [(var,Int)]
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

-- | A monomial multiplied by a constant
data Term coeff var  
  = Term !coeff !(Monom var) 
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)
  
-- | Polynomials as linear combination of monomials
newtype GenPoly coeff var 
  = GenPoly [Term coeff var]
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

-- | Polynomial expressions
data Expr v
  = VarE !v
  | KstE !Integer
  | NegE (Expr v)
  | LinE [(Sign,Expr v)]
  | MulE [Expr v]
  | PowE (Expr v) !Int
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

--------------------------------------------------------------------------------
-- * Evaluation to @Num@

evalSign :: Num c => Sign -> c -> c
evalSign Plus  = id
evalSign Minus = negate

evalMonom :: Num c => (var -> c) -> Monom var -> c
evalMonom f (Monom list) = product (map g list) where
  g (var,expo) = (f var)^expo

evalTerm :: Num c => (coeff -> c) -> (var -> c) -> Term coeff var -> c
evalTerm f g (Term coeff monom) = f coeff * evalMonom g monom  

evalGenPoly :: Num c => (coeff -> c) -> (var -> c) -> GenPoly coeff var -> c
evalGenPoly f g (GenPoly terms) = sum (map (evalTerm f g) terms)
 
evalExpr :: Num c => (var -> c) -> Expr var -> c
evalExpr evalVar = go where
  go expr = case expr of
    VarE v   -> evalVar v
    KstE k   -> fromInteger k
    NegE e   -> negate (go e)
    LinE xs  -> sum [ evalSign pm (go x) | (pm,x) <- xs ]
    MulE xs  -> product (map go xs)
    PowE e k -> (go e)^k
    
--------------------------------------------------------------------------------

{-

prettExpr :: Expr -> String
prettExpr expr = DList.toList (prettPrecExpr 0 expr)

prettyPrecExpr :: Int -> Expr -> DList Char
prettyPrecExpr = go where

  chr c = DList.singleton c
  str s = DList.append s

  go !prec !expr = case expr of
    VarE !Var
    KstE !Integer
    NegE Expr
    LinE [(Sign,Expr)]
    MulE [Expr]
    PowE Expr !Int

-}

--------------------------------------------------------------------------------

