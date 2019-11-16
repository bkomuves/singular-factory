
-- | Brute-force counting of solutions of polynomial equations over (small) finite field

{-# LANGUAGE BangPatterns, TypeApplications, ScopedTypeVariables, DataKinds #-}
module Counting where

--------------------------------------------------------------------------------

import Data.Proxy

import Domains
import Polynomial
import GFTables

--------------------------------------------------------------------------------

{-
counting_main = do
  tryAndInitGFTables

  let [x,y,z] = map var [1..3]
  let f = - y^2*z + x^3 + 2*z^3 :: Poly V Integer
  print f
  
  let [x,y,z,u,v,w] = map var [1..6]
  let g1 = x*v - y*u 
      g2 = x*w - z*u 
      g3 = y*w - z*v
      gs = [g1,g2,g3] :: [Poly V Integer]
-}

{-
  cnts  @(FF 5) (map mapIntoDomain gs)
  cntsP @(FF 5) (map mapIntoDomain gs)
-}

{-  
  putStrLn "==========================\np=2"
  cnt @(FF 2)       (mapIntoDomain f)
  cnt @(GF 2 2 "x") (mapIntoDomain f)
  cnt @(GF 2 3 "x") (mapIntoDomain f)
  cnt @(GF 2 4 "x") (mapIntoDomain f)
  cnt @(GF 2 5 "x") (mapIntoDomain f)
  cnt @(GF 2 6 "x") (mapIntoDomain f)
  cnt @(GF 2 7 "x") (mapIntoDomain f)


  putStrLn "==========================\np=3"
  cnt @(FF 3)       (mapIntoDomain f)
  cnt @(GF 3 2 "x") (mapIntoDomain f)
  cnt @(GF 3 3 "x") (mapIntoDomain f)
  cnt @(GF 3 4 "x") (mapIntoDomain f)
  cnt @(GF 3 5 "x") (mapIntoDomain f)
  cnt @(GF 3 6 "x") (mapIntoDomain f)
  cnt @(GF 3 7 "x") (mapIntoDomain f)
-}

{-
  putStrLn "==========================\np=5"
  cntP @(FF 5)       (mapIntoDomain f)
  cntP @(GF 5 2 "x") (mapIntoDomain f)
  cntP @(GF 5 3 "x") (mapIntoDomain f)
  cntP @(GF 5 4 "x") (mapIntoDomain f)
  cntP @(GF 5 5 "x") (mapIntoDomain f)
-}

{-
  putStrLn "==========================\np=7"
  cntP @(FF 7)       (mapIntoDomain f)
  cntP @(GF 7 2 "x") (mapIntoDomain f)
  cntP @(GF 7 3 "x") (mapIntoDomain f)
  cntP @(GF 7 4 "x") (mapIntoDomain f)
  cntP @(GF 7 5 "x") (mapIntoDomain f)

cnt :: forall domain. FiniteDomain domain => Poly V domain -> IO ()
cnt poly = do
  let cnt = countAffineHypersurface 3 poly
  let q = domainSize (Proxy :: Proxy domain)
  print (q,cnt,divMod (cnt-1) (q-1))

cntP :: forall domain. FiniteDomain domain => Poly V domain -> IO ()
cntP poly = do
  let cnt = countProjectiveHypersurface 3 poly
  let q = domainSize (Proxy :: Proxy domain)
  print (q,cnt)

cnts :: forall domain. FiniteDomain domain => [Poly V domain] -> IO ()
cnts polys = do
  let cnt = countAffineSolutions 6 polys
  let q = domainSize (Proxy :: Proxy domain)
  print (q,cnt,divMod (cnt-1) (q-1))

cntsP :: forall domain. FiniteDomain domain => [Poly V domain] -> IO ()
cntsP polys = do
  let cnt = countProjectiveSolutions 6 polys
  let q = domainSize (Proxy :: Proxy domain)
  print (q,cnt)

-}

--------------------------------------------------------------------------------
-- * Hypersurfaces

-- | Count points of a hypersurface in an affine space
-- over a finite field.
--
-- The @Int@ input is the number of variables (that is, the dimension).
--
countAffineHypersurface :: FiniteDomain domain => Int -> Poly vars domain -> Int
countAffineHypersurface = go where
  go !1 !p = countTrues [ polyIsZero (substitute1 1 (konst a) p) | a <- enumerateDomain ]    
  go !k !p = sum        [ go (k-1)   (substitute1 k (konst a) p) | a <- enumerateDomain ]

-- | Count points of a hypersurface in an projective space
-- over a finite field.
-- 
-- The @Int@ input is the number of /variables/ (that is, the dimension plus one)!
--
-- NOTE: We assume that the input is a homogeneous polynomial, but this is not checked!! 
countProjectiveHypersurface :: FiniteDomain domain => Int -> Poly vars domain -> Int
countProjectiveHypersurface = go False where
  go !isPos !1 !p = countTrues [ polyIsZero $ substitute1 1 (konst a) p | a <- projEnumerateDomain isPos , let b = isPos || not (isZero a) , b ]
  go !isPos !k !p = sum        [ go b (k-1) $ substitute1 k (konst a) p | a <- projEnumerateDomain isPos , let b = isPos || not (isZero a)     ]
    
--------------------------------------------------------------------------------
-- * Systems of equations (varieties)

-- | Count solutions of a system of polynomial equations in an affine space
-- over a finite field.
--
-- The @Int@ input is the number of variables (that is, the dimension).
countAffineSolutions :: FiniteDomain domain => Int -> [Poly vars domain] -> Int
countAffineSolutions = go where
  go !1 !ps = countTrues [ and $ map polyIsZero $ map (substitute1 1 (konst a)) ps | a <- enumerateDomain ]    
  go !k !ps = sum        [ go (k-1)             $ map (substitute1 k (konst a)) ps | a <- enumerateDomain ]

-- | Count solutions of a system of polynomial equations in a projective space
-- over a finite field.
--
-- The @Int@ input is the number of /variables/ (that is, the dimension plus one)!
-- 
-- NOTE: We assume that the input is a list of homogeneous polynomial, but this is not checked!! 
countProjectiveSolutions :: FiniteDomain domain => Int -> [Poly vars domain] -> Int
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
