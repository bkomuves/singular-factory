
-- | Bindings to singular-factory

{-# LANGUAGE BangPatterns, DataKinds, TypeSynonymInstances, FlexibleInstances #-}
module Factory where

--------------------------------------------------------------------------------

import Data.List
import Data.Ratio
import Data.Char
import Data.Word

import Control.Monad

import GHC.TypeLits
import Data.Proxy

import System.IO.Unsafe as Unsafe

import Numeric.GMP.Types
import qualified Numeric.GMP.Utils as GMP 

import CanonicalForm
import DList as DList
import GFTables

--------------------------------------------------------------------------------
-- * tests

pk :: [Var] -> Int -> CF
pk vars k = Unsafe.unsafePerformIO $ do
  terms <- sequence [ varPowCF v k | v <- vars ]
  return $ foldl1' (+) (1 : terms)

pkn :: [Var] -> Int -> CF
pkn vars k = Unsafe.unsafePerformIO $ do
  terms <- sequence [ varPowCF v k | v <- vars ]
  return $ 1 - foldl1' (+) terms

factory_main = do
  tryAndInitGFTables
  
  char <- getCharacteristic 
  putStrLn $ "current characteristic = " ++ show char

{-
  cf <- makeIntegerCF 12345
  print =<< getZZ cf

  cf <- makeIntegerCF 2106444751876036
  print =<< getZZ cf

  cf <- makeRationalCF (123142414918234249 / 7324433294752894)
  print =<< getQQ cf
-}

  vars <- mapM newVar [1..3]
  let p1 = pk vars 1
      p2 = pk vars 2
      p3 = pk vars 3
      p4 = pk vars 4
  let test1 = p1^4 - 1
      test2 = p2^2 - 1
      test3 = substitute (last vars) p3 test2
      test4 = p1^4 * p2^4 - 1
      test5 = p1^8 * p2^8 * p3^8 - 1
      test6 = sum [ (pk vars k)^10 * (pkn vars k)^10 - 1 | k <-[1..10] ]
      test7 = p1^9 * p2^9 * p3^9 
      test8 = p1^9 - 1

  putStrLn "factory test"

  setCharacteristic1 32003     -- 19 
  char <- getCharacteristic 
  putStrLn $ "current characteristic = " ++ show char

  test8 <- mapInto test8
  let what = test8

  putStrLn "\ninput:"
  printCF what

  putStrLn "\nfactors:"
  facs <- factorize what
  forM facs $ \(fac,expo) -> do
    putStr $ "expo = " ++ show expo ++ " | " 
    printCF fac

  putStrLn "\nsanity chack:"
  let test = product $ map (\(fac,expo) -> fac^expo) facs
  printCF (test - what)

--------------------------------------------------------------------------------

-- | The maximum prime characteristic Factory can handle
maxCharacteristic :: Int
maxCharacteristic = 536870909     -- 2^29-3

--------------------------------------------------------------------------------
-- * Basic operations and instances

instance Num CF where
  fromInteger n = Unsafe.unsafePerformIO $ newSmallConstCF $ fromIntegral n     -- BIG INTS ARE NOT HANDLED!!!
  (+) x y = Unsafe.unsafePerformIO (plusIO  x y)
  (-) x y = Unsafe.unsafePerformIO (minusIO x y)
  (*) x y = Unsafe.unsafePerformIO (timesIO x y)
  abs    = error "CF: Num/abs is not implemented"
  signum = error "CF: Num/signum is not implemented"

eqCF :: CF -> CF -> Bool
eqCF x y = Unsafe.unsafePerformIO (isEqualIO x y)

{- there is already an instance... -}
-- instance Eq CF where (==) = eqCF

substitute :: Var -> CF -> (CF -> CF)
substitute var what cf = Unsafe.unsafePerformIO (substituteIO var what cf)

printCF :: CF -> IO ()
printCF cf = do
  terms <- genericMarshalFromCF makeTerm cf
  --print terms
  putStrLn $ intercalate " + " (map prettyTerm terms)

--------------------------------------------------------------------------------
-- * Base domains

data BaseValue
  = ZZ !Integer
  | QQ !Rational
  | GF !Int
  | FF !Int
  deriving (Eq,Ord,Show)

baseValueIsOne :: BaseValue -> Bool
baseValueIsOne val = case val of
  ZZ n   ->  n == 1
  QQ q   ->  q == 1
  GF k   ->  k == 1
  FF k   ->  k == 1

prettyBaseValue :: BaseValue -> String
prettyBaseValue val = case val of
  ZZ n -> show n
  QQ q -> case denominator q of
            1 -> show (numerator q)
            b -> show (numerator q) ++ "/" ++ show b
  GF k -> "<" ++ show k ++ ">"
  FF k -> "<" ++ show k ++ ">"

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
prettyMonom (Monom [] ) = "(1)"
prettyMonom (Monom ves) = intercalate "*" (map f ves) where
  f (v,0) = ""
  f (v,1) = g v
  f (v,k) = g v ++ "^" ++ show k
  g level = [chr (96 + level)]

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

--------------------------------------------------------------------------------
-- * Marshalling from CF

getZZ :: CF -> IO (Maybe Integer)
getZZ cf = isInZZ cf >>= \b -> case b of
  False -> return Nothing
  True  -> isImmediate cf >>= \b -> case b of
    True  -> (Just . fromIntegral) <$> getSmallIntValue cf
    False -> Just <$> getGmpNumerator cf
      -- error "getZZ: bignums are not implemented yet"

getQQ :: CF -> IO (Maybe Rational)
getQQ cf = isInQQ cf >>= \b -> case b of
  False -> return Nothing
  True  -> do
    -- Just numer <- (getZZ =<< getNumer cf)
    -- Just denom <- (getZZ =<< getDenom cf)
    numer <- getGmpNumerator   cf
    denom <- getGmpDenominator cf
    return $ Just $ (numer % denom)

getGF :: CF -> IO (Maybe Int)
getGF cf = isInGF cf >>= \b -> case b of
  False -> return Nothing
  True  -> isImmediate cf >>= \b -> case b of
    True  -> (Just . fromIntegral) <$> getSmallIntValue cf
    False -> error "getGF: bignums are not implemented yet"

getFF :: CF -> IO (Maybe Int)
getFF cf = isInFF cf >>= \b -> case b of
  False -> return Nothing
  True  -> isImmediate cf >>= \b -> case b of
    True  -> (Just . fromIntegral) <$> getSmallIntValue cf
    False -> error "getFF: bignums are not implemented yet"

getBaseValue :: CF -> IO (Maybe BaseValue)
getBaseValue cf = isInBaseDomain cf >>= \b -> case b of
  False -> return Nothing
  True  -> getZZ cf >>= \mb -> case mb of
    Just n  -> return $ Just (ZZ n)
    Nothing -> getQQ cf >>= \mb -> case mb of
      Just q  -> return $ Just (QQ q)
      Nothing -> getFF cf >>= \mb -> case mb of
        Just k  -> return $ Just (FF k)
        Nothing -> getGF cf >>= \mb -> case mb of
          Just k  -> return $ Just (GF k)
          Nothing -> return Nothing

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
          isZero this >>= \b -> if b
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

{-
--------------------------------------------------------------------------------
-- * high level

-- | Prime field
newtype Fp (p :: Nat) = Fp Int

-- | Finite field
newtype GF (q :: Nat) = GF Int

class BaseDomain a 

instance BaseDomain Integer
instance BaseDomain Rational
instance KnownNat p => BaseDomain (Fp p)
instance KnownNat q => BaseDomain (GF q)

data BaseDomain c => Trie c (n :: Nat) where
  Zero :: KnownNat n => Trie c n
  One  :: KnownNat n => Trie c n
  Base :: c -> Trie c 0
  Poly :: KnownNat n => IntMap (Trie c n) -> Trie c (n + 1)

--------------------------------------------------------------------------------
-- * low level

newtype Level 
  = Level Int 
  deriving (Eq,Ord,Show)

-- | Haskell data type roughly corresponding to CanonicalForm 
data Value
  = ValZ  !Integer
  | ValQ  !Rational
  | ValGF !Int
  | ValPoly !Level !(Array Int CF)
  deriving (Eq,Ord,Show)

data CanonicalForm

type CF = ForeignPtr CanonicalForm

marshalFromCPP :: CF -> IO Value

marshalToCPP   :: Value -> IO CF
-}
