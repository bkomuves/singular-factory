
-- | High-level bindings to singular-factory

{-# LANGUAGE 
      BangPatterns, PatternSynonyms, KindSignatures, DataKinds,
      FlexibleInstances, TypeSynonymInstances, ScopedTypeVariables,
      EmptyDataDecls
  #-}
module Math.Singular.Factory.Polynomial where

--------------------------------------------------------------------------------

import GHC.TypeLits
import Data.Proxy

import System.IO.Unsafe as Unsafe

import Math.Singular.Factory.Internal.CanonicalForm
import Math.Singular.Factory.Internal.Factory

import Math.Singular.Factory.Variables
import Math.Singular.Factory.Domains

--------------------------------------------------------------------------------
-- * Polynomials

-- | A multivariate polynomial over a base domain.
--
-- Typically, you want to fix your variable set (see the module "Math.Singular.Factory.Variables"), 
-- make a type synonym, and use that; for example:
--
-- > type Poly domain = Polynomial (VarN "x") domain
--
newtype Polynomial varset domain 
  = Poly { unPoly :: CF }

instance Eq (Polynomial vars domain) where
  (==) (Poly cf1) (Poly cf2) = safeEqCF cf1 cf2

instance forall vars domain. VariableSet vars => Show (Polynomial vars domain) where
  show (Poly cf) = showCF_with (varIdxName (Proxy :: Proxy vars)) cf
  
polyIsZero :: Polynomial vars domain -> Bool
polyIsZero (Poly cf) = isZeroCF cf

polyIsOne :: Polynomial vars domain -> Bool
polyIsOne (Poly cf) = isOneCF cf

-- | Returns true if the polynomial is a constant  
inBaseDomain :: Polynomial vars domain -> Bool
inBaseDomain (Poly cf) = isInBaseDomainCF cf

-- | If it is a constant, returns the value
mbConstant :: BaseDomain domain => Polynomial vars domain -> Maybe domain
mbConstant (Poly cf) = if isInBaseDomainCF cf 
  then Just (unsafeCfToBase cf)
  else Nothing

-- | A constant polynomial
konst :: BaseDomain domain => domain -> Polynomial vars domain
konst = Poly . baseToCF

-- | A variable as a polynomial
var :: VarIdx -> Polynomial vars domain
var idx = Poly $ varCF (theNthVar idx)

-- | A power of a variable
varPow :: VarIdx -> Int -> Polynomial vars domain
varPow idx expo = Poly $ varPowCF (theNthVar idx) expo 

--------------------------------------------------------------------------------
-- * Operations on polynomials

mapIntoDomain 
  :: forall domain1 domain2 vars. (BaseDomain domain1, BaseDomain domain2) 
  => Polynomial vars domain1 -> Polynomial vars domain2
mapIntoDomain (Poly cf) = result where
  result = Poly (mapIntoCF (factoryChar pxy) cf)
  pxy    = Proxy :: Proxy domain2

instance forall vars domain. BaseDomain domain => Num (Polynomial vars domain) where
  fromInteger = Poly . (baseToCF :: domain -> CF) . fromInteger
  negate (Poly cf) = Poly (negate cf)
  (Poly cf1) + (Poly cf2) = Poly (cf1 + cf2)
  (Poly cf1) - (Poly cf2) = Poly (cf1 - cf2)
  (Poly cf1) * (Poly cf2) = Poly (cf1 * cf2)
  abs    = id
  signum = const 1
  
pow :: BaseDomain domain => Polynomial vars domain -> Int -> Polynomial vars domain
pow (Poly cf) expo = Poly $ powCF cf expo

-- | Polynomial GCD
polyGCD :: BaseDomain domain => Polynomial vars domain -> Polynomial vars domain -> Polynomial vars domain 
polyGCD (Poly cf1) (Poly cf2) = Poly $ gcdPolyCF cf1 cf2

-- | Polynomial reduction
polyReduce :: BaseDomain domain => Polynomial vars domain -> Polynomial vars domain -> Polynomial vars domain 
polyReduce (Poly cf1) (Poly cf2) = Poly $ reduceCF cf1 cf2

-- | Polynomial factorization 
factorize :: BaseDomain domain => Polynomial vars domain -> [(Polynomial vars domain, Int)]
factorize (Poly cf) = map f (factorizeCF cf) where
  f (p, expo) = (Poly p, expo)

{-
-- | Polynomial factorization (the new algorithms by Martin Lee)
factorizeNew :: forall vars domain. BaseDomain domain => Polynomial vars domain -> [(Polynomial vars domain, Int)]
factorizeNew (Poly cf) = 
  case factoryChar (Proxy :: Proxy domain) of
    CharZero   -> map f (ratFactorizeCF cf alphaOne True)
    CharFp p   -> map f (fpFactorizeCF  cf          True)
    CharGF p n -> map f (gfFactorizeCF  cf          True)
  where
    f (p, expo) = (Poly p, expo)
    alphaOne = theNthVar 1          -- const Variable& v= Variable (1)   
  
-- | Polynomial factorization (old way)
factorizeOld :: BaseDomain domain => Polynomial vars domain -> [(Polynomial vars domain, Int)]
factorizeOld (Poly cf) = map f (oldFactorizeCF cf) where
  f (p, expo) = (Poly p, expo)
-}

-- | Substitution
substitute1 :: BaseDomain domain => VarIdx -> Polynomial vars domain -> Polynomial vars domain -> Polynomial vars domain
substitute1 idx (Poly what) (Poly cf) = Poly (substituteCF (theNthVar idx) what cf)

-- | Evaluate a polynomial at the given point
evaluate :: BaseDomain domain => (VarIdx -> domain) -> Polynomial vars domain -> domain
evaluate fun (Poly cf0) = unsafeCfToBase (go 1 cf0) where
  go :: Int -> CF -> CF
  go !idx !cf = if isInBaseDomainCF cf
    then cf
    else go (idx+1) (substituteCF (theNthVar idx) (baseToCF $ fun idx) cf)

--------------------------------------------------------------------------------
