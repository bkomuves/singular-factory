
-- | Medium-level bindings to singular-factory

{-# LANGUAGE BangPatterns, DataKinds, TypeSynonymInstances, FlexibleInstances #-}
module Math.Singular.Factory.Internal.Factory where

--------------------------------------------------------------------------------

import Data.List
import Data.Maybe
import Data.Ratio
import Data.Char
import Data.Word

import Control.Monad

import GHC.TypeLits
import Data.Proxy

import System.IO.Unsafe as Unsafe

import Numeric.GMP.Types
import qualified Numeric.GMP.Utils as GMP 

import Math.Singular.Factory.Internal.CanonicalForm
import Math.Singular.Factory.Internal.DList as DList

--------------------------------------------------------------------------------

-- | The maximum prime characteristic Factory can handle
-- (note: for prime power fields the limit is much smaller)
maxCharacteristic :: Int
maxCharacteristic = 536870909     -- 2^29-3

--------------------------------------------------------------------------------
-- * Version info

factoryVersion :: String
factoryVersion = Unsafe.unsafePerformIO getFactoryVersion

packageVersion :: String
packageVersion = Unsafe.unsafePerformIO getPackageVersion

--------------------------------------------------------------------------------
-- * Basic operations and instances

-- | Because the native equality comparison seems to be unreliable,
-- we provide an alternative implementation which subtracts the two
-- polynomials and then tests for the result being zero...
safeEqCF :: CF -> CF -> Bool
safeEqCF x y = isZeroCF (x - y)

-- | Note: this does not seem to be reliable in practice...
-- Better subtract them and then use isZeroCF...
nativeEqCF :: CF -> CF -> Bool
nativeEqCF x y = Unsafe.unsafePerformIO (isEqualIO x y)

{- there is already an instance... -}
-- instance Eq CF where (==) = eqCF

isZeroCF :: CF -> Bool
isZeroCF cf = Unsafe.unsafePerformIO (isZeroIO cf)

isOneCF :: CF -> Bool
isOneCF cf = Unsafe.unsafePerformIO (isOneIO cf)
 
varCF :: Var -> CF
varCF var = Unsafe.unsafePerformIO (varIO var)

varPowCF :: Var -> Int -> CF
varPowCF var expo = Unsafe.unsafePerformIO (varPowIO var expo)

instance Num CF where
  fromInteger n = Unsafe.unsafePerformIO $ newSmallConstCF $ fromIntegral n     -- BIG INTS ARE NOT HANDLED!!!
  (+) x y = Unsafe.unsafePerformIO (plusIO  x y)
  (-) x y = Unsafe.unsafePerformIO (minusIO x y)
  (*) x y = Unsafe.unsafePerformIO (timesIO x y)
  abs    = error "CF: Num/abs is not implemented"
  signum = error "CF: Num/signum is not implemented"

powCF :: CF -> Int -> CF
powCF x n = Unsafe.unsafePerformIO (powIO x n)

modCF :: CF -> CF -> CF
modCF x y = Unsafe.unsafePerformIO (modIO x y)

divCF :: CF -> CF -> CF
divCF x y = Unsafe.unsafePerformIO (divIO x y)

substituteCF :: Var -> CF -> (CF -> CF)
substituteCF var what cf = Unsafe.unsafePerformIO (substituteIO var what cf)

gcdPolyCF :: CF -> CF -> CF
gcdPolyCF x y = Unsafe.unsafePerformIO (gcdPolyIO x y)

reduceCF :: CF -> CF -> CF
reduceCF x y = Unsafe.unsafePerformIO (reduceIO x y)

factorizeCF :: CF -> [(CF,Int)]
factorizeCF x = Unsafe.unsafePerformIO (factorizeIO x)

--------------------------------------------------------------------------------
-- * pretty printing

showCF :: CF -> String
showCF x = Unsafe.unsafePerformIO (showIO x)

showCF_with :: (Int -> String) -> CF -> String
showCF_with showVar x = Unsafe.unsafePerformIO (showIO_with showVar x)

showCF_dense :: CF -> String
showCF_dense x = Unsafe.unsafePerformIO (showIO_dense x)

printCF :: CF -> IO ()
printCF cf = putStrLn =<< showIO cf

showIO :: CF -> IO String
showIO cf = do
  terms <- genericMarshalFromCF makeTerm cf
  return $ intercalate " + " (map prettyTerm terms)

showIO_with :: (Int -> String) -> CF -> IO String
showIO_with showVar cf = do
  terms <- genericMarshalFromCF makeTerm cf
  return $ intercalate " + " (map (prettyTermWith showVar) terms)

showIO_dense :: CF -> IO String
showIO_dense cf = do
  terms <- genericMarshalFromCF makeTerm cf
  return $ intercalate "+" (map prettyTerm terms)

--------------------------------------------------------------------------------
-- * Galois fields

-- | Values in a Galois field.
--
-- They can either 0, or a power of the canonical generator (of the multiplicative
-- group, which is cyclic).
-- 
-- Furthermore, they can be possibly also element of the prime subfield.
--
data GFValue
  = GFZero
  | GFSubField { _gfGenExpo :: !Int , _gfModP :: !Int } 
  | GFGenPow   { _gfGenExpo :: !Int }
  deriving (Eq,Ord)

instance Show GFValue where
  show = showGFValue1 "#"

-- | Elements of the prime subfield are shown as numbers, the rest as
-- powers of the generator
showGFValue1 :: String -> GFValue -> String
showGFValue1 gen gfv = case gfv of
  GFZero               -> "0"
  GFSubField expo modp -> show modp
  GFGenPow   expo      -> if expo == 1 then gen else gen ++ "^" ++ show expo

-- | Elements of the prime subfield are also shown as powers of the generator
showGFValue2 :: String -> GFValue -> String
showGFValue2 gen gfv = case gfv of
  GFZero               -> "0"
  GFSubField expo modp -> if expo == 1 then gen else gen ++ "^" ++ show expo
  GFGenPow   expo      -> if expo == 1 then gen else gen ++ "^" ++ show expo

--------------------------------------------------------------------------------
-- * Base domains
            
data BaseValue
  = ZZ !Integer
  | QQ !Rational
  | FF !Int
  | GF !GFValue
  deriving (Eq,Ord,Show)

baseValueIsOne :: BaseValue -> Bool
baseValueIsOne val = case val of
  ZZ n   ->  n == 1
  QQ q   ->  q == 1
  GF gfv ->  case gfv of { GFSubField _ 1 -> True ; _ -> False }
  FF k   ->  k == 1

prettyBaseValue :: BaseValue -> String
prettyBaseValue val = case val of
  ZZ n -> show n
  QQ q -> case denominator q of
            1 -> show (numerator q)
            b -> show (numerator q) ++ "/" ++ show b
  GF k -> show k -- "<" ++ show k ++ ">"
  FF k -> show k -- "<" ++ show k ++ ">"

--------------------------------------------------------------------------------
-- * Monomials

type Level = Int
type Expo  = Int

newtype Monom 
  = Monom [(Level,Expo)] 
  deriving (Eq,Ord,Show)

monomIsNull :: Monom -> Bool
monomIsNull (Monom list) = null list

prettyMonom :: Monom -> String
prettyMonom = prettyMonomWith g where
  g level = [chr (96 + level)]

prettyMonomWith :: (Int -> String) -> Monom -> String
prettyMonomWith showVar (Monom [] ) = "1" -- "(1)"
prettyMonomWith showVar (Monom ves) = intercalate "*" (map f ves) where
  f (v,0) = "1"
  f (v,1) = showVar v
  f (v,k) = showVar v ++ "^" ++ show k  

----------------------------------------
-- * coefficients

type Coeff = BaseValue           -- TEMPORARY HACK

coeffIsOne :: Coeff -> Bool
coeffIsOne = baseValueIsOne

prettyCoeff = prettyBaseValue

----------------------------------------
-- * Terms

data Term 
  = Term !Coeff !Monom
  deriving (Eq,Ord,Show)

prettyTerm :: Term -> String
prettyTerm (Term coeff monom) 
  | coeffIsOne  coeff  = prettyMonom monom
  | monomIsNull monom  = prettyCoeff coeff
  | otherwise          = prettyCoeff coeff ++ "*" ++ prettyMonom monom

prettyTermWith :: (Int -> String) -> Term -> String
prettyTermWith showVar (Term coeff monom) 
  | coeffIsOne  coeff  = prettyMonomWith showVar monom
  | monomIsNull monom  = prettyCoeff coeff
  | otherwise          = prettyCoeff coeff ++ "*" ++ prettyMonomWith showVar monom

--------------------------------------------------------------------------------
-- * Marshalling from CF

getZZ :: CF -> IO (Maybe Integer)
getZZ cf = isInZZ_IO cf >>= \b -> case b of
  False -> return Nothing
  True  -> isImmediateIO cf >>= \b -> case b of
    True  -> (Just . fromIntegral) <$> getSmallIntValue cf
    False -> Just <$> getGmpNumerator cf
      -- error "getZZ: bignums are not implemented yet"

getQQ :: CF -> IO (Maybe Rational)
getQQ cf = isInQQ_IO cf >>= \b -> case b of
  False -> return Nothing
  True  -> do
    -- Just numer <- (getZZ =<< getNumer cf)
    -- Just denom <- (getZZ =<< getDenom cf)
    numer <- getGmpNumerator   cf
    denom <- getGmpDenominator cf
    return $ Just $ (numer % denom)

getFF :: CF -> IO (Maybe Int)
getFF cf = isInFF_IO cf >>= \b -> case b of
  False -> return Nothing
  True  -> isImmediateIO cf >>= \b -> case b of
    True  -> (Just . fromIntegral) <$> getSmallIntValue cf
    False -> error "getFF: bignums are not implemented yet"

getGF :: CF -> IO (Maybe GFValue)
getGF cf = isInGF_IO cf >>= \b -> case b of
  False -> return Nothing
  True  -> isFFinGF_IO cf >>= \b -> case b of
    True  -> do 
      k <- getSmallIntValue cf 
      e <- getGFValue cf
      return $ Just $ if (k==0) then GFZero else (GFSubField e k)
    False -> do
      e <- getGFValue cf
      return $ Just $ GFGenPow e
      
getBaseValue :: CF -> IO (Maybe BaseValue)
getBaseValue cf = isInBaseDomainIO cf >>= \b -> case b of
  False -> return Nothing
  True  -> getBaseValueNotGF cf >>= \mb -> case mb of
    Just val -> return $ Just val
    Nothing  -> getGF cf >>= \mb -> case mb of
      Just gf  -> return $ Just (GF gf)
      Nothing  -> return Nothing

getBaseValueNotGF :: CF -> IO (Maybe BaseValue)
getBaseValueNotGF cf = isInBaseDomainIO cf >>= \b -> case b of
  False -> return Nothing
  True  -> getZZ cf >>= \mb -> case mb of
    Just n  -> return $ Just (ZZ n)
    Nothing -> getQQ cf >>= \mb -> case mb of
      Just q  -> return $ Just (QQ q)
      Nothing -> getFF cf >>= \mb -> case mb of
        Just k  -> return $ Just (FF k)
        Nothing -> return Nothing
          
--------------------------------------------------------------------------------

isInBaseDomainCF :: CF -> Bool
isInBaseDomainCF cf = Unsafe.unsafePerformIO (isInBaseDomainIO cf)

isInCoeffDomainCF :: CF -> Bool
isInCoeffDomainCF cf = Unsafe.unsafePerformIO (isInCoeffDomainIO cf)

isInPolyDomainCF :: CF -> Bool
isInPolyDomainCF cf = Unsafe.unsafePerformIO (isInPolyDomainIO cf)

isInQuotDomainCF :: CF -> Bool
isInQuotDomainCF cf = Unsafe.unsafePerformIO (isInQuotDomainIO cf)

isInExtensionCF :: CF -> Bool
isInExtensionCF cf = Unsafe.unsafePerformIO (isInExtensionIO cf)

--------------------------------------------------------------------------------

valueZZ :: CF -> Integer
valueZZ cf = case Unsafe.unsafePerformIO (getZZ cf) of
  Just n  -> n
  Nothing -> error "valueZZ: not an integer"

valueQQ :: CF -> Rational
valueQQ cf = case Unsafe.unsafePerformIO (getQQ cf) of
  Just n  -> n
  Nothing -> error "valueQQ: not a rational"

valueGF :: CF -> GFValue
valueGF cf = case Unsafe.unsafePerformIO (getGF cf) of
  Just n  -> n
  Nothing -> error "valueGF: not a GF element"

valueFF :: CF -> Int
valueFF cf = case Unsafe.unsafePerformIO (getFF cf) of
  Just n  -> n
  Nothing -> error "valueFF: not a FF element"

--------------------------------------------------------------------------------

makeMonom :: [(Level,Expo)] -> Monom
makeMonom ves = Monom $ filter cond ves where cond (lev,expo) = expo > 0

makeTerm :: [(Level,Expo)] -> BaseValue -> Term
makeTerm ves cf = Term cf (makeMonom ves)

genericMarshalFromCF :: ([(Level,Expo)] -> BaseValue -> a) -> CF -> IO [a]
genericMarshalFromCF user cf = DList.toList <$> (genericMarshalFromCF_dlist user cf)

--------------------------------------------------------------------------------

genericMarshalFromCF_dlist :: ([(Level,Expo)] -> BaseValue -> a) -> CF -> IO (DList a)
genericMarshalFromCF_dlist user = worker [] where
  worker expos cf = do
    getBaseValue cf >>= \mb -> case mb of
      Just val -> return (DList.singleton $ user expos val)
      Nothing  -> do
        level <- getLevel  cf
        deg   <- getDegree cf
        stuff <- forM [0..deg] $ \d -> do
          this <- getCfAtIndex cf d
          isZeroIO this >>= \b -> if b
            then return (DList.empty)
            else if d > 0 
              then worker ((level,d):expos) this
              else worker            expos  this
        return $ DList.concat stuff

{-
genericMarshalFromCF_list :: ([(Level,Expo)] -> BaseValue -> a) -> CF -> IO [a]
genericMarshalFromCF_list user = worker [] where
  worker expos cf = do
    getBaseValue cf >>= \mb -> case mb of
      Just val -> return [user expos val]
      Nothing  -> do
        level <- getLevel  cf
        deg   <- getDegree cf
        stuff <- forM [0..deg] $ \d -> do
          this <- getCfAtIndex cf d
          isZero this >>= \b -> if b
            then return []
            else if d > 0 
              then worker ((level,d):expos) this
              else worker            expos  this
        return $ Data.List.concat stuff
-}

--------------------------------------------------------------------------------

marshalUnivariateFromCF :: CF -> IO [(CF,Int)]
marshalUnivariateFromCF = worker where
  worker cf = do
    deg <- getDegree cf
    case deg of
      0 -> return [(cf,0)]
      _ -> do
        -- level <- getLevel  cf
        mbs <- forM [0..deg] $ \d -> do
          this <- getCfAtIndex cf d
          isZeroIO this >>= \b -> if b
            then return $ Nothing
            else return $ Just (this,d)
        return $ catMaybes mbs
        
--------------------------------------------------------------------------------
