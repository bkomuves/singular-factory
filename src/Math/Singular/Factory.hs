{- |

Haskell bindings to the Singular-Factory multivariate polynomial factorization engine.

Singular is a computer algebra system developed at the University of Kaiserslautern, 
and Factory is the polynomial factorization engine of Singular.

It handles multivariate polynomials over the integers, rationals, and finite fields
(and also algebraic and trancendental extensions of these, but those are not yet
supported).

Links: 

* Singular: https://www.singular.uni-kl.de/

* Factory: https://www.singular.uni-kl.de/dox/html/factory_page.html

-}

module Math.Singular.Factory
  ( -- * Initialization
    initialize
    -- * Configuration
  , factoryVersion 
  , factoryConfig , FactoryConfig(..)
  , printVersion
  , printConfig   
    -- * Re-exported modules
  , module Math.Singular.Factory.Domains
  , module Math.Singular.Factory.Variables
  , module Math.Singular.Factory.Polynomial
  ) 
  where

--------------------------------------------------------------------------------
  
import Math.Singular.Factory.Domains
import Math.Singular.Factory.Variables
import Math.Singular.Factory.Polynomial

import Math.Singular.Factory.GFTables
import Math.Singular.Factory.Internal.Factory
import Math.Singular.Factory.Internal.CanonicalForm

--------------------------------------------------------------------------------

-- | You are supposed to call 'initialize' before doing anything
-- (though factorization over characteristic zeros should work without it)
initialize = do
  initGFTables
  setDefaultSwitches

--------------------------------------------------------------------------------

data FactoryConfig = FactoryConfig
  { withFLINT :: Bool
  , withNTL   :: Bool
  , withGMP   :: Bool
  }
  deriving (Eq,Show)
  
factoryConfig :: FactoryConfig
factoryConfig = FactoryConfig
  { withFLINT   = haveFLINT
  , withNTL     = haveNTL
  , withGMP     = haveGMP
  }
  
--------------------------------------------------------------------------------

-- | Prints factory\'s version
printVersion = do
  putStrLn $ "factory version = " ++ factoryVersion  

-- | Prints some more detailed configuration info
printConfig = do
  putStrLn $ "factory version = " ++ factoryVersion  
  putStrLn $ "package version = " ++ packageVersion  
  mb_gftables <- getGFTablesDir 
  putStrLn $ "gftables folder = " ++ show mb_gftables
  putStrLn $ "have FLINT = " ++ show haveFLINT
  putStrLn $ "have NTL   = " ++ show haveNTL
  putStrLn $ "have GMP   = " ++ show haveGMP
  
--------------------------------------------------------------------------------

  
  