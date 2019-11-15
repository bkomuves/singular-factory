
-- | High-level bindings to singular-factory

{-# LANGUAGE 
      BangPatterns, PatternSynonyms, KindSignatures, DataKinds,
      FlexibleInstances, TypeSynonymInstances, ScopedTypeVariables
  #-}
module Polynomial where

--------------------------------------------------------------------------------

import GHC.TypeLits
import Data.Proxy

{-}
import Data.Sequence ( Seq , (<|) , (|>) , ViewL(..) , ViewR(..) )
import qualified Data.Sequence as Seq 
-}

import System.IO.Unsafe as Unsafe

import GFTables ( tryAndInitGFTables , initGFTables )

import CanonicalForm
import Factory
import Domains

--------------------------------------------------------------------------------

data V = V
type T = Poly V Integer -- (GF 3 2 "x") -- Integer

poly_main = do
  tryAndInitGFTables
  
  let [x,y,z] = map var [1,2,3]
  let p1 = (1+x+y+z)       :: T
      p2 = (1+x*x+y*y+z*z) :: T
      pk k = 1+x^k+y^k+z^k :: T

  let p = (pk 3) ^ 4 - 1 
      
  putStrLn "" 
  print p

  putStrLn "" 
  mapM_ print $ factorize p

  putStrLn "" 
  print $ p - product [ q^e | (q,e) <- factorize p ]
  
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

--------------------------------------------------------------------------------
-- * Polynomials

-- | A multivariate polynomial over a base domain
newtype Poly varset domain  = Poly CF

instance Eq (Poly vars domain) where
  (==) (Poly cf1) (Poly cf2) = cf1 == cf2

instance Show (Poly vars domain) where
  show (Poly cf) = showCF cf
  
instance forall vars domain. BaseDomain domain => Num (Poly vars domain) where
  fromInteger = Poly . (baseToCF :: domain -> CF) . fromInteger
  negate (Poly cf) = Poly (negate cf)
  (Poly cf1) + (Poly cf2) = Poly (cf1 + cf2)
  (Poly cf1) - (Poly cf2) = Poly (cf1 - cf2)
  (Poly cf1) * (Poly cf2) = Poly (cf1 * cf2)
  abs    = id
  signum = const 1
  
-- | A variable
var :: VarIdx -> Poly vars domain
var idx = Poly $ varCF (theFactoryVars !! (idx-1)) 

-- | A power of a variable
varPow :: VarIdx -> Int -> Poly vars domain
varPow idx expo = Poly $ varPowCF (theFactoryVars !! (idx-1)) expo 

pow :: BaseDomain domain => Poly vars domain -> Int -> Poly vars domain
pow (Poly cf) expo = Poly $ powCF cf expo

-- | Polynomial GCD
polyGCD :: BaseDomain domain => Poly vars domain -> Poly vars domain -> Poly vars domain 
polyGCD (Poly cf1) (Poly cf2) = Poly $ gcdPolyCF cf1 cf2

-- | Polynomial reduction
polyReduce :: BaseDomain domain => Poly vars domain -> Poly vars domain -> Poly vars domain 
polyReduce (Poly cf1) (Poly cf2) = Poly $ reduceCF cf1 cf2

factorize :: BaseDomain domain => Poly vars domain -> [(Poly vars domain, Int)]
factorize (Poly cf) = map f (factorizeCF cf) where
  f (p, expo) = (Poly p, expo)

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

lowerVarList :: [String]
lowerVarList = map (:[]) abc ++ [ ys ++ [y] | ys<-lowerVarList , y<-abc ] where
  abc = ['a'..'z']

upperVarList :: [String]
upperVarList = map (:[]) abc ++ [ ys ++ [y] | ys<-lowerVarList , y<-abc ] where
  abc = ['A'..'Z']

--------------------------------------------------------------------------------

