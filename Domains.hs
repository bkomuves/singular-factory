
-- | Base domains.
--
-- These are the base rings and fields Factory can work with, namely:
--
-- * the ring integers
-- * the ring of rationals
-- * finite fields (prime fields and Galois fields)
--
-- Note: as Factory has the base domain as a global state, this whole library is
-- not at all thread safe!
--

{-# LANGUAGE 
      BangPatterns, PatternSynonyms, KindSignatures, DataKinds,
      FlexibleInstances, TypeSynonymInstances
  #-}
module Domains where

--------------------------------------------------------------------------------

import Data.Ratio

import GHC.TypeLits
import Data.Proxy
import Data.IORef

import Control.Monad
import System.IO.Unsafe as Unsafe

import CanonicalForm
--  ( Var , CF , setCharacteristic1 , setCharacteristic2 , setCharacteristic3 )
import Factory hiding ( FF , GF )
import GFTables ( tryAndInitGFTables , initGFTables )

--------------------------------------------------------------------------------

domains_main = do
  tryAndInitGFTables
  print $ [ fromIntegral i :: Fp 5  | i<-[0..30] ]
  print $ [ mkFF i :: FF 5          | i<-[0..30] ]
  print $ [ mkGF i :: GF 5 2 "x"    | i<-[0..30] ]
  print $ [ genPowGF i :: GF 5 2 "x"    | i<-[0..30] ]
  
  let one = mkGF 1  :: GF 19 3 "y" 
      a   = genGF   :: GF 19 3 "y"
      a2  = a*a
      a3  = a*a*a
      ra3 = 1/a3
  
  print (one,a,a2,a3,ra3,a3*ra3)

--------------------------------------------------------------------------------
-- * The global characteristics

type Prime = Int

data FactoryChar
  = CharZero               -- ^ QQ
  | CharFp !Prime          -- ^ prime field
  | CharGF !Prime !Int     -- ^ Galois field
  deriving (Eq,Show)

-- | Unfortunately, Factory maintains a global state...
theFactoryChar :: IORef FactoryChar
theFactoryChar = Unsafe.unsafePerformIO $ newIORef CharZero

setFactoryChar :: FactoryChar -> IO ()
setFactoryChar new = do
  old <- readIORef theFactoryChar
  when (new /= old) $ do
    writeIORef theFactoryChar new
    case new of 
      CharZero   -> setCharacteristic1 0           
      CharFp p   -> setCharacteristic1 p
      CharGF p n -> setCharacteristic3 p n '@'      -- we use '@' for the Galois field variable for now
      
--------------------------------------------------------------------------------
-- * Prime fields

-- | Haskell prime fields
newtype Fp (p :: Nat) = Fp Int deriving (Eq)
  
fpPrime :: KnownNat p => Fp p -> Int
fpPrime = fromIntegral . natVal . proxyP where
  proxyP :: Fp p -> Proxy p
  proxyP _ = Proxy 

modp :: (KnownNat p, Integral a) => a -> Fp p
modp k = fp where
  fp = Fp $ mod (fromIntegral k) (fpPrime fp)

-- proxyFp :: Fp p -> Proxy p
-- proxyFp _ = Proxy

instance Show (Fp p) where
  show (Fp k) = show k

instance KnownNat p => Num (Fp p) where
  fromInteger     = modp
  negate fp@(Fp k)   = if k==0 then Fp 0 else Fp (fpPrime fp - k) 
  (Fp a) + (Fp b) = modp (a+b)
  (Fp a) - (Fp b) = modp (a-b)
  (Fp a) * (Fp b) = modp (a*b)
  abs    = id
  signum = const (Fp 1)

fpToFF :: KnownNat p => Fp p -> FF p
fpToFF (Fp k) = mkFF k

--------------------------------------------------------------------------------
-- * Finite fields

-- | Factory prime fields
newtype FF (p :: Nat)   
  = FF { unFF :: CF }

mkFF :: (KnownNat p, Integral a) => a -> FF p 
mkFF !k = ff where
  ff = Unsafe.unsafePerformIO $ do
    setBaseDomain (mkProxy ff)
    cf <- (mapInto =<< makeIntegerCF (fromIntegral k))
    return (FF cf)
    
instance Eq (FF p) where 
  (FF a) == (FF b) = eqCF a b

instance Show (FF p) where 
  show (FF cf) = show (valueFF cf) -- "[" ++ show (valueFF cf) ++ "]"

ffPrime :: KnownNat p => FF p -> Int
ffPrime = fromIntegral . natVal . proxyP where
  proxyP :: FF p -> Proxy p
  proxyP _ = Proxy

instance KnownNat p => Num (FF p) where
  fromInteger     = mkFF
  negate (FF a)   = FF (negate a)
  (FF a) + (FF b) = FF (a + b)
  (FF a) - (FF b) = FF (a - b)
  (FF a) * (FF b) = FF (a * b)
  abs    = id
  signum = const 1

instance (KnownNat p) => Fractional (FF p) where
  fromRational q  = mkFF (numerator q) / mkFF (denominator q)
  (FF a) / (FF b) = FF (divCF a b)
  
--------------------------------------------------------------------------------
-- * Galois fields

-- | Galois fields @GF(p^n)@. 
--
-- The (nonzero) elements are represented as powers of the canonical generator.
--
-- The symbol is the name of the canonical generator (used for pretty-printing).
newtype GF (p :: Nat) (n :: Nat) (x :: Symbol) 
  = GF { unGF :: CF }

-- | Create elements of the prime subfield. For the rest, you can use the
-- powers of the generator.
mkGF :: (KnownNat p, KnownNat n, KnownSymbol x, Integral a) => a -> GF p n x
mkGF !k = gf where
  gf = Unsafe.unsafePerformIO $ do
    setBaseDomain (mkProxy gf)
    cf <- (mapInto =<< makeIntegerCF (fromIntegral k))
    return (GF cf)

-- | The canonical generator of the (multiplicative group of the) Galois field 
genGF :: (KnownNat p, KnownNat n, KnownSymbol x) => GF p n x
genGF = gf where
  gf = Unsafe.unsafePerformIO $ do
    setBaseDomain (mkProxy gf)
    cf <- getGFGenerator
    return (GF cf)

-- | A power of the canonical generator 
genPowGF :: (KnownNat p, KnownNat n, KnownSymbol x) => Int -> GF p n x
genPowGF e = powGF genGF e 

powGF :: (KnownNat p, KnownNat n, KnownSymbol x) => GF p n x -> Int -> GF p n x
powGF (GF cf) e = GF (powCF cf e)

instance Eq (GF p n x) where 
  (GF a) == (GF b)  =  eqCF a b

instance (KnownNat p, KnownNat n, KnownSymbol x) => Show (GF p n x) where 
  show gf@(GF cf) = showGFValue1 (gfSymbol gf) (valueGF cf)

instance (KnownNat p, KnownNat n, KnownSymbol x) => Num (GF p n x) where
  fromInteger     = mkGF
  negate (GF a)   = GF (negate a)
  (GF a) + (GF b) = GF (a + b)
  (GF a) - (GF b) = GF (a - b)
  (GF a) * (GF b) = GF (a * b)
  abs    = id
  signum = const 1
  
instance (KnownNat p, KnownNat n, KnownSymbol x) => Fractional (GF p n x) where
  fromRational q  = mkGF (numerator q) / mkGF (denominator q)
  (GF a) / (GF b) = GF (divCF a b)

gfPrime :: KnownNat p => GF p n x -> Int
gfPrime = fromIntegral . natVal . proxyP where
  proxyP :: GF p n x -> Proxy p
  proxyP _ = Proxy
  
gfExponent :: KnownNat n => GF p n x -> Int
gfExponent = fromIntegral . natVal . proxyE where
  proxyE :: GF p n x -> Proxy n
  proxyE _ = Proxy
  
gfSymbol :: KnownSymbol x => GF p n x -> String
gfSymbol = symbolVal . proxyX where
  proxyX :: GF p n x -> Proxy x
  proxyX _ = Proxy

  
--------------------------------------------------------------------------------
-- * Base domains

class (Eq a, Show a, Num a) => BaseDomain a where
  characteristic    :: Proxy a -> Int
  charExponent      :: Proxy a -> Int 
  baseDomainName    :: Proxy a -> String
  factoryChar       :: Proxy a -> FactoryChar
  baseToCF          :: a -> CF
  
setBaseDomain :: BaseDomain a => Proxy a -> IO ()
setBaseDomain = setFactoryChar . factoryChar  

instance BaseDomain Integer where
  characteristic _    = 0
  charExponent   _    = 1
  baseDomainName _    = "ZZ"
  factoryChar    _    = CharZero
  baseToCF       x    = Unsafe.unsafePerformIO (makeIntegerCF x)
  
instance BaseDomain Rational where
  characteristic _    = 0
  charExponent   _    = 1
  baseDomainName _    = "QQ"
  factoryChar     _   = CharZero
  baseToCF       x    = Unsafe.unsafePerformIO (makeRationalCF x)

instance KnownNat p => BaseDomain (Fp p) where
  characteristic pxy  = (fpPrime $ proxyUndef pxy)
  charExponent   _    = 1
  baseDomainName pxy  = "F_" ++ show (characteristic pxy) 
  factoryChar    pxy  = CharFp (characteristic pxy)
  baseToCF       x    = baseToCF (fpToFF x)

instance KnownNat p => BaseDomain (FF p) where
  characteristic pxy  = (ffPrime $ proxyUndef pxy)
  charExponent   pxy  = 1
  baseDomainName pxy  = "FF(" ++ show (characteristic pxy) ++ ")" where
  factoryChar    pxy  = CharFp (characteristic pxy) 
  baseToCF   (FF cf)  = cf
    
instance (KnownNat q, KnownNat n, KnownSymbol x) => BaseDomain (GF q n x) where
  characteristic pxy  = (gfPrime    $ proxyUndef pxy)
  charExponent   pxy  = (gfExponent $ proxyUndef pxy)
  baseDomainName pxy  = "GF(" ++ show (characteristic pxy) ++ ")" where
  factoryChar    pxy  = CharGF (characteristic pxy) (charExponent pxy)
  baseToCF   (GF cf)  = cf

--------------------------------------------------------------------------------

mkProxy :: a -> Proxy a
mkProxy _ = Proxy

proxyUndef :: Proxy a -> a
proxyUndef _ = undefined

--------------------------------------------------------------------------------
