
-- | The test-suite

{-# LANGUAGE BangPatterns, DataKinds, DeriveFunctor #-}
module Main where

--------------------------------------------------------------------------------

import Data.List

import Control.Monad
import System.Random

import Test.Tasty
import Test.Tasty.HUnit

import Math.Singular.Factory.GFTables ( initGFTables )

import Math.Singular.Factory.Domains
import Math.Singular.Factory.Polynomial
import Math.Singular.Factory.Counting

--------------------------------------------------------------------------------

main = do
  initGFTables  
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests"  
  [ unit_tests
--  , randomized_tests
  ]

unit_tests :: TestTree
unit_tests = testGroup "Unit tests"
  [ testCase "reconstruction for some polys over Z"      (assertBool "failed" $ reconstr_some_polys some_polys_ZZ)
  , testCase "reconstruction for some polys over F_2"    (assertBool "failed" $ reconstr_some_polys some_polys_F2)
  , testCase "reconstruction for some polys over F_3"    (assertBool "failed" $ reconstr_some_polys some_polys_F3)
  , testCase "reconstruction for some polys over F_5"    (assertBool "failed" $ reconstr_some_polys some_polys_F5)
  , testCase "reconstruction for some polys over GF(4)"  (assertBool "failed" $ reconstr_some_polys some_polys_GF4)
  , testCase "reconstruction for some polys over GF(8)"  (assertBool "failed" $ reconstr_some_polys some_polys_GF8)
  , testCase "reconstruction for some polys over GF(9)"  (assertBool "failed" $ reconstr_some_polys some_polys_GF9)
  ]  
    
--------------------------------------------------------------------------------

type Poly domain = Polynomial VarAbc domain 

some_polys_v1_ZZ :: [Poly Integer]
some_polys_v1_ZZ = 
  [ (x^2 + 1) * (x^5 + 1) * (x^7 + 1)
  
  , x^2 - y^2
  , y^2 - z^2
  , x^5 - y^5
  , x^7 - y^7
  , x^10 - y^10
 
  , (1 + x + y + z)^2
  , (1 + x + y + z)^4
  , (1 + x + y + z)^8
  
  , (1 + x + y + z)^2 - 1
  , (1 + x + y + z)^4 - 1
  , (1 + x + y + z)^8 - 1
  
  , (1 + x^2 + y^2 + z^2)^2 - 1
  , (1 + x^2 + y^2 + z^2)^4 - 1
  , (1 + x^2 + y^2 + z^2)^8 - 1  

  , ((1 - x - y - z)^2 - 1) * ((1 - x - y - z )^2 + 1) 
  , ((1 + x + y + z)^2 - 1) * ((1 - x - y - z )^2 + 1) 
  , ((1 + x + y + z)^2 - 1) * ((1 + x + y + z )^2 + 1) 
  ] 
  where
    myvars@[x,y,z,u,v,w] = map var [1..6]

-- | These seem to be too big for finite fields except GF(2) ???
some_polys_v2_ZZ :: [Poly Integer]
some_polys_v2_ZZ =
  [ (x-13) * (x+27) * (x-42)^2 * (x+7)^3 * (x-11)^5
  , ( (1+x+y+z)^4 + 1 ) * ( (1+x+y+z)^4 + 2 ) 
  , ( (1+x+y+z)^5 + 1 ) * ( (1+x+y+z)^5 + 2 ) 
  , ( (1+x+y+z)^7 + 1 ) * ( (1+x+y+z)^7 + 2 ) 
  , ( (1+x+y+z)^10 + 1 ) * ( (1+x+y+z)^10 + 2 ) 
  , ( (1+x+y+z)^20 + 1 ) * ( (1+x+y+z)^20 + 2 ) 
  ] 
  where
    myvars@[x,y,z,u,v,w] = map var [1..6]

some_polys_ZZ :: [Poly Integer]
some_polys_ZZ = concat
  [ some_polys_v1_ZZ
  , some_polys_v2_ZZ
  ] 
  
--------------------------------------------------------------------------------


some_polys_F2 :: [Poly (FF 2)]
some_polys_F2 = map mapIntoDomain some_polys_ZZ

some_polys_F3 :: [Poly (FF 3)]
some_polys_F3 = map mapIntoDomain some_polys_v1_ZZ

some_polys_F5 :: [Poly (FF 5)]
some_polys_F5 = map mapIntoDomain some_polys_v1_ZZ

some_polys_GF4 :: [Poly (GF 2 2 "q")]
some_polys_GF4 = map mapIntoDomain some_polys_v1_ZZ

some_polys_GF8 :: [Poly (GF 2 3 "q")]
some_polys_GF8 = map mapIntoDomain some_polys_v1_ZZ

some_polys_GF9 :: [Poly (GF 3 2 "q")]
some_polys_GF9 = map mapIntoDomain some_polys_v1_ZZ

--------------------------------------------------------------------------------
-- TODO: collect examples where the factorization is known

data KnownFactors poly = KnownFactors
  { _poly :: poly
  , _facs :: [(poly,Int)]
  } 
  deriving (Show,Functor)

-- A bivariate polynomial over F2  
bivarF2_1 = KnownFactors
  { _poly = "x^4120 + x^4118*y^2 + x^3708*y^400 + x^3706*y^402+ x^2781*y^1300 + x^2779*y^1302 + x^1339*y^2700+ x^927*y^3100 + y^4000 + x^7172*y^4167 + x^8349*y^4432+ x^8347*y^4434 + x^6760*y^4567 + x^5833*y^5467+ x^5568*y^7132 + x^11401*y^8599"
  , _facs = [ ("x^5568*y^4432 + x^1339 + x^927*y^400 + y^1300" , 1)
            , ("x^5833*y^4167 + x^2781 + x^2779*y^2 + y^2700"  , 1)
            ]
  }
    
--------------------------------------------------------------------------------

reconstructFromFactors :: BaseDomain dom => [(Poly dom, Int)] -> Poly dom 
reconstructFromFactors = product . map (uncurry pow)

prop_reconstr_from_factors :: BaseDomain dom => Poly dom -> Bool
prop_reconstr_from_factors p  =  (p == reconstructFromFactors (factorize p))

reconstr_some_polys :: BaseDomain dom => [Poly dom] -> Bool
reconstr_some_polys list = and (map prop_reconstr_from_factors list)

--------------------------------------------------------------------------------
