
-- | Bindings to singular-factory

{-# LANGUAGE BangPatterns, DataKinds, TypeSynonymInstances, FlexibleInstances #-}
module Factory where

--------------------------------------------------------------------------------

import Data.List
import Data.Ratio
import Data.Char

import Control.Monad

import GHC.TypeLits
import Data.Proxy

import System.IO.Unsafe as Unsafe

import CanonicalForm

--------------------------------------------------------------------------------

pk :: [Var] -> Int -> CF
pk vars k = Unsafe.unsafePerformIO $ do
  terms <- sequence [ varPowCF v k | v <- vars ]
  return $ foldl1' (+) (1 : terms)

factory_main = do
  vars <- mapM newVar [1..3]
  let p1 = pk vars 1
      p2 = pk vars 2
      p3 = pk vars 3
      p4 = pk vars 4
  let test1 = p1^4 - 1
      test2 = p1^4 * p2^4 - 1
  putStrLn "factory test"

  putStrLn "\ninput:"
  printCF test1
  putStrLn "\nfactors:"
  facs <- factorize test1
  forM facs $ \(fac,expo) -> do
    putStr $ "expo = " ++ show expo ++ " | " 
    printCF fac

--------------------------------------------------------------------------------

instance Num CF where
  fromInteger n = Unsafe.unsafePerformIO $ newSmallConstCF $ fromIntegral n     -- BIG INTS ARE NOT HANDLED!!!
  (+) x y = Unsafe.unsafePerformIO (plusIO  x y)
  (-) x y = Unsafe.unsafePerformIO (minusIO x y)
  (*) x y = Unsafe.unsafePerformIO (timesIO x y)
  abs    = error "CF: Num/abs is not implemented"
  signum = error "CF: Num/signum is not implemented"

printCF :: CF -> IO ()
printCF cf = do
  terms <- genericMarshalFromCF makeTerm cf
  --print terms
  putStrLn $ intercalate " + " (map prettyTerm terms)

--------------------------------------------------------------------------------

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

type Coeff = BaseValue           -- TEMPORARY HACK

coeffIsOne :: Coeff -> Bool
coeffIsOne = baseValueIsOne

prettyCoeff = prettyBaseValue

----------------------------------------

data Term 
  = Term !Coeff !Monom
  deriving (Eq,Ord,Show)

prettyTerm :: Term -> String
prettyTerm (Term coeff monom) 
  | coeffIsOne  coeff  = prettyMonom monom
  | monomIsNull monom  = prettyCoeff coeff
  | otherwise          = prettyCoeff coeff ++ "*" ++ prettyMonom monom

--------------------------------------------------------------------------------

getZZ :: CF -> IO (Maybe Integer)
getZZ cf = isInZZ cf >>= \b -> case b of
  False -> return Nothing
  True  -> isImmediate cf >>= \b -> case b of
    True  -> (Just . fromIntegral) <$> getSmallIntValue cf
    False -> error "getZZ: bignums are not implemented yet"

getQQ :: CF -> IO (Maybe Rational)
getQQ cf = isInQQ cf >>= \b -> case b of
  False -> return Nothing
  True  -> do
    Just numer <- (getZZ =<< getNumer cf)
    Just denom <- (getZZ =<< getDenom cf)
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
genericMarshalFromCF user = worker [] where
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
        return $ concat stuff

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
