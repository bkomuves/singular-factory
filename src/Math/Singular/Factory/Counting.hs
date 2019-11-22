
-- | Brute-force counting of solutions of polynomial equations over (small) finite field

{-# LANGUAGE BangPatterns, TypeApplications, ScopedTypeVariables, DataKinds #-}
module Math.Singular.Factory.Counting where

--------------------------------------------------------------------------------

import Data.Proxy

import Math.Singular.Factory.Domains
import Math.Singular.Factory.Polynomial
import Math.Singular.Factory.GFTables

--------------------------------------------------------------------------------
-- * Hypersurfaces

-- | Count points of a hypersurface in an affine space
-- over a finite field.
--
-- The @Int@ input is the number of variables (that is, the dimension).
--
countAffineHypersurface :: FiniteDomain domain => Int -> Polynomial vars domain -> Int
countAffineHypersurface = go where
  go !1 !p = countTrues [ polyIsZero (substitute1 1 (konst a) p) | a <- enumerateDomain ]    
  go !k !p = sum        [ go (k-1)   (substitute1 k (konst a) p) | a <- enumerateDomain ]

-- | Count points of a hypersurface in an projective space
-- over a finite field.
-- 
-- The @Int@ input is the number of /variables/ (that is, the dimension plus one)!
--
-- NOTE: We assume that the input is a homogeneous polynomial, but this is not checked!! 
countProjectiveHypersurface :: FiniteDomain domain => Int -> Polynomial vars domain -> Int
countProjectiveHypersurface = go False where
  go !isPos !1 !p = countTrues [ polyIsZero $ substitute1 1 (konst a) p | a <- projEnumerateDomain isPos , let b = isPos || not (isZero a) , b ]
  go !isPos !k !p = sum        [ go b (k-1) $ substitute1 k (konst a) p | a <- projEnumerateDomain isPos , let b = isPos || not (isZero a)     ]
    
--------------------------------------------------------------------------------
-- * Systems of equations (varieties)

-- | Count solutions of a system of polynomial equations in an affine space
-- over a finite field.
--
-- The @Int@ input is the number of variables (that is, the dimension).
countAffineSolutions :: FiniteDomain domain => Int -> [Polynomial vars domain] -> Int
countAffineSolutions = go where
  go !1 !ps = countTrues [ and $ map polyIsZero $ map (substitute1 1 (konst a)) ps | a <- enumerateDomain ]    
  go !k !ps = sum        [ go (k-1)             $ map (substitute1 k (konst a)) ps | a <- enumerateDomain ]

-- | Count solutions of a system of polynomial equations in a projective space
-- over a finite field.
--
-- The @Int@ input is the number of /variables/ (that is, the dimension plus one)!
-- 
-- NOTE: We assume that the input is a list of homogeneous polynomial, but this is not checked!! 
countProjectiveSolutions :: FiniteDomain domain => Int -> [Polynomial vars domain] -> Int
countProjectiveSolutions = go False where
  go !isPos !1 !ps = countTrues [ and $ map polyIsZero $ map (substitute1 1 (konst a)) ps | a <- projEnumerateDomain isPos , let b = isPos || not (isZero a) , b ]
  go !isPos !k !ps = sum        [ go b (k-1)           $ map (substitute1 k (konst a)) ps | a <- projEnumerateDomain isPos , let b = isPos || not (isZero a)     ]

--------------------------------------------------------------------------------
-- * Misc helpers

-- | if the input is False, return [0,1], otherwise all elements of the domain
projEnumerateDomain :: FiniteDomain domain => Bool -> [domain]
projEnumerateDomain True  = enumerateDomain
projEnumerateDomain False = [0,1]

countTrues :: [Bool] -> Int
countTrues = foldr f 0 where
  f False !c = c
  f True  !c = c+1
  
--------------------------------------------------------------------------------
