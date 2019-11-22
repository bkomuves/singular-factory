
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
import Math.Singular.Factory.Domains

--------------------------------------------------------------------------------
-- * Variables

-- | A variable index. 
--
-- In factory, there is a single linear sequence of variables.
-- Variables are indexed starting from 1.
-- 
type VarIdx = Int

-- | In factory, there is a single linear sequence of variables.
-- We \"precalculate\" these (lazily).
theFactoryVars :: [Var]
theFactoryVars = map mk [1..] where
  mk i = Unsafe.unsafePerformIO $ newVarL i

theNthVar :: VarIdx -> Var
theNthVar idx = theFactoryVars !! (idx-1)

--------------------------------------------------------------------------------
-- * Polynomials

-- | A multivariate polynomial over a base domain.
--
-- Typically, you want to fix your variable set (see below), make a type synonym, and use that:
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
-- * Variable  sets    

-- | The class of variable sets. Since Factory only supports a single linear
-- variable set, these differ only by naming conventions.
--
class VariableSet v where
  varIdxName :: Proxy v -> VarIdx -> String
  
-- | The variable set @x1, x2, x3, x4...@ (where \"x\" can be any string)
data VarN  (s :: Symbol)   

-- | The variable set @x_1, x_2, x_3, x_4...@ (where \"x\" can be any string)
data Var_N (s :: Symbol)           

-- | The variable set @x[1], x[2], x[3], x[4]...@ (where \"x\" can be any string)
data VarBracketN (s :: Symbol)     

-- | The variable set @a, b, c, d...@
data VarAbc    

-- | The variable set @A, B, C, D...@
data VarABC    

-- | The variable set @x, y, z, u, v, w, a, b, c...@
data VarXyz    

-- | The variable set @X, Y, Z, U, V, W, A, B, C...@
data VarXYZ    

instance forall s. KnownSymbol s => VariableSet (VarN s) where
  varIdxName _ = indexedVars (symbolVal (Proxy :: Proxy s))

instance forall s. KnownSymbol s => VariableSet (Var_N s) where
  varIdxName _ = indexedVarsUnderscore (symbolVal (Proxy :: Proxy s))

instance forall s. KnownSymbol s => VariableSet (VarBracketN s) where
  varIdxName _ = indexedVarsBracket (symbolVal (Proxy :: Proxy s))

instance VariableSet VarAbc where
  varIdxName _ = abcVars
  
instance VariableSet VarABC where
  varIdxName _ = capitalAbcVars

instance VariableSet VarXyz where
  varIdxName _ = xyzVars
  
instance VariableSet VarXYZ where
  varIdxName _ = capitalXyzVars
  
--------------------------------------------------------------------------------
-- * Standard naming conventions of variables

-- | Eg. @x1, x2, x3...@
indexedVars :: String -> VarIdx -> String
indexedVars prefix = \i -> prefix ++ show i

-- | Eg. @x_1, x_2, x_3...@
indexedVarsUnderscore :: String -> VarIdx -> String
indexedVarsUnderscore prefix = \i -> prefix ++ "_" ++ show i

-- | Eg. @x[1], x[2], x[3]...@
indexedVarsBracket :: String -> VarIdx -> String
indexedVarsBracket prefix = \i -> prefix ++ "[" ++ show i ++ "]"

-- | That is, @a, b, c...@
abcVars :: VarIdx -> String
abcVars idx = lowerVarList !! (idx-1)

-- | That is, @A, B, C...@
capitalAbcVars :: VarIdx -> String
capitalAbcVars idx = lowerVarList !! (idx-1)

-- | @x, y, z, u, v, w, a, b, c ... , t@
xyzVars :: VarIdx -> String
xyzVars idx = [ "xyzuvwabcdefghijklmnopqrst" !! (idx-1) ]

capitalXyzVars :: VarIdx -> String
capitalXyzVars idx = [ "XYZUVWABCDEFGHIJKLMNOPQRST" !! (idx-1) ]

-- | The infinite list of variables @a, b ..., z, aa, ab, ac, ...@
lowerVarList :: [String]
lowerVarList = map (:[]) abc ++ [ ys ++ [y] | ys<-lowerVarList , y<-abc ] where
  abc = ['a'..'z']

upperVarList :: [String]
upperVarList = map (:[]) abc ++ [ ys ++ [y] | ys<-lowerVarList , y<-abc ] where
  abc = ['A'..'Z']

--------------------------------------------------------------------------------

