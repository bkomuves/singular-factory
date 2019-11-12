
-- | Low-level bindings to singular-factory

{-# LANGUAGE BangPatterns, EmptyDataDecls, ForeignFunctionInterface #-}
module CanonicalForm where

--------------------------------------------------------------------------------

import Data.Ratio

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array

--------------------------------------------------------------------------------

data Variable
data CanonicalForm
data Factor               -- CFFactor
data FactorList           -- List<CFFactor>

type Var     = ForeignPtr Variable
type CF      = ForeignPtr CanonicalForm
type Fac     = ForeignPtr Factor
type FacList = ForeignPtr FactorList

--------------------------------------------------------------------------------

foreign import ccall "&free_var"
  varFinalizerPtr :: FunPtr (Ptr Variable -> IO ()) 

foreign import ccall "&free_cf"
  cfFinalizerPtr :: FunPtr (Ptr CanonicalForm -> IO ()) 

foreign import ccall "&free_fac"
  facFinalizerPtr :: FunPtr (Ptr Factor -> IO ()) 

foreign import ccall "&free_faclist"
  faclistFinalizerPtr :: FunPtr (Ptr FactorList -> IO ()) 

makeVar :: Ptr Variable -> IO Var
makeVar = newForeignPtr varFinalizerPtr

makeCF :: Ptr CanonicalForm -> IO CF
makeCF = newForeignPtr cfFinalizerPtr

makeFac :: Ptr Factor -> IO Fac
makeFac = newForeignPtr facFinalizerPtr

makeFacList :: Ptr FactorList -> IO FacList
makeFacList = newForeignPtr faclistFinalizerPtr

--------------------------------------------------------------------------------

foreign import ccall "new_var" c_new_var :: CInt -> IO (Ptr Variable)

newVar :: Int -> IO Var
newVar level = makeVar =<< c_new_var (fromIntegral level)

--------------------------------------------------------------------------------

foreign import ccall "get_factor" c_get_factor :: Ptr Factor -> IO (Ptr CanonicalForm)

getFactorCF :: Fac -> IO CF
getFactorCF fac = withForeignPtr fac $ \ptr -> makeCF =<< c_get_factor ptr

foreign import ccall "get_fac_expo" c_get_fac_expo :: Ptr Factor -> IO CInt

getFactorExpo :: Fac -> IO Int
getFactorExpo fac = withForeignPtr fac $ \ptr -> fromIntegral <$> c_get_fac_expo ptr

unpackFactor :: Fac -> IO (CF,Int)
unpackFactor fac = do
  cf   <- getFactorCF   fac
  expo <- getFactorExpo fac
  return (cf,expo)

--------------------------------------------------------------------------------

foreign import ccall "get_list_length" c_get_list_length :: Ptr FactorList -> IO CInt

getListLength :: FacList -> IO Int
getListLength faclist = withForeignPtr faclist $ \ptr -> fromIntegral <$> c_get_list_length ptr

foreign import ccall "flatten_faclist" c_flatten_faclist :: Ptr FactorList -> Ptr (Ptr Factor) -> IO ()

flattenFactorList :: FacList -> IO [Fac]
flattenFactorList faclist = withForeignPtr faclist $ \listptr -> do
  n <- fromIntegral <$> c_get_list_length listptr
  allocaArray n $ \arrptr -> do
    c_flatten_faclist listptr arrptr
    facptrs <- peekArray n arrptr
    mapM makeFac facptrs

unpackFactorList :: FacList -> IO [(CF,Int)]
unpackFactorList faclist = mapM unpackFactor =<< flattenFactorList faclist

foreign import ccall "my_factorize" c_factorize :: Ptr CanonicalForm -> Ptr FactorList

factorize' :: CF -> IO FacList
factorize' cf = withForeignPtr cf $ \ptr -> makeFacList (c_factorize ptr)

factorize :: CF -> IO [(CF,Int)]
factorize cf = unpackFactorList =<< factorize' cf

--------------------------------------------------------------------------------

foreign import ccall "empty_cf" c_empty_cf :: IO (Ptr CanonicalForm)
 
newEmptyCF :: IO CF
newEmptyCF = makeCF =<< c_empty_cf

foreign import ccall "const_cf" c_const_cf :: CInt -> IO (Ptr CanonicalForm)
 
newSmallConstCF :: Int -> IO CF
newSmallConstCF k = makeCF =<< c_const_cf (fromIntegral k)

foreign import ccall "var_cf"     c_var_cf     :: Ptr Variable ->        IO (Ptr CanonicalForm)
foreign import ccall "var_pow_cf" c_var_pow_cf :: Ptr Variable -> Int -> IO (Ptr CanonicalForm)

varCF :: Var -> IO CF
varCF var = withForeignPtr var $ \vptr -> makeCF =<< c_var_cf vptr

varPowCF :: Var -> Int -> IO CF
varPowCF var expo = withForeignPtr var $ \vptr -> makeCF =<< c_var_pow_cf vptr expo

--------------------------------------------------------------------------------

liftBool :: IO CInt -> IO Bool
liftBool action = action >>= \k -> return (k/=0)

foreign import ccall "is_zero" c_is_zero :: Ptr CanonicalForm -> IO CInt
foreign import ccall "is_one"  c_is_one  :: Ptr CanonicalForm -> IO CInt

isZero :: CF -> IO Bool
isZero cf = withForeignPtr cf $ \ptr -> liftBool (c_is_zero ptr)

isOne :: CF -> IO Bool
isOne cf = withForeignPtr cf $ \ptr -> liftBool (c_is_one ptr)

foreign import ccall "is_imm"        c_is_imm        :: Ptr CanonicalForm -> IO CInt
foreign import ccall "is_univariate" c_is_univariate :: Ptr CanonicalForm -> IO CInt

isImmediate :: CF -> IO Bool
isImmediate cf = withForeignPtr cf $ \ptr -> liftBool (c_is_imm ptr)

isUnivariate :: CF -> IO Bool
isUnivariate cf = withForeignPtr cf $ \ptr -> liftBool (c_is_univariate ptr)

foreign import ccall "in_ZZ" c_in_ZZ :: Ptr CanonicalForm -> IO CInt
foreign import ccall "in_QQ" c_in_QQ :: Ptr CanonicalForm -> IO CInt
foreign import ccall "in_GF" c_in_GF :: Ptr CanonicalForm -> IO CInt
foreign import ccall "in_FF" c_in_FF :: Ptr CanonicalForm -> IO CInt

isInZZ :: CF -> IO Bool
isInZZ cf = withForeignPtr cf $ \ptr -> liftBool (c_in_ZZ ptr)

isInQQ :: CF -> IO Bool
isInQQ cf = withForeignPtr cf $ \ptr -> liftBool (c_in_QQ ptr)

isInGF :: CF -> IO Bool
isInGF cf = withForeignPtr cf $ \ptr -> liftBool (c_in_GF ptr)

isInFF :: CF -> IO Bool
isInFF cf = withForeignPtr cf $ \ptr -> liftBool (c_in_FF ptr)

foreign import ccall "in_BaseDomain"  c_in_BaseDomain  :: Ptr CanonicalForm -> IO CInt
foreign import ccall "in_CoeffDomain" c_in_CoeffDomain :: Ptr CanonicalForm -> IO CInt
foreign import ccall "in_PolyDomain"  c_in_PolyDomain  :: Ptr CanonicalForm -> IO CInt

isInBaseDomain :: CF -> IO Bool
isInBaseDomain cf = withForeignPtr cf $ \ptr -> liftBool (c_in_BaseDomain ptr)

isInCoeffDomain :: CF -> IO Bool
isInCoeffDomain cf = withForeignPtr cf $ \ptr -> liftBool (c_in_CoeffDomain ptr)

isInPolyDomain :: CF -> IO Bool
isInPolyDomain cf = withForeignPtr cf $ \ptr -> liftBool (c_in_PolyDomain ptr)

foreign import ccall "smallint_value" c_smallint_value :: Ptr CanonicalForm -> IO CInt
foreign import ccall "degree_of"      c_degree_of      :: Ptr CanonicalForm -> IO CInt
foreign import ccall "level_of"       c_level_of       :: Ptr CanonicalForm -> IO CInt

getSmallIntValue :: CF -> IO Int
getSmallIntValue cf = withForeignPtr cf $ \ptr -> fromIntegral <$> (c_smallint_value ptr)

getDegree :: CF -> IO Int
getDegree cf = withForeignPtr cf $ \ptr -> fromIntegral <$> (c_degree_of ptr)

getLevel :: CF -> IO Int
getLevel cf = withForeignPtr cf $ \ptr -> fromIntegral <$> (c_level_of ptr)

foreign import ccall "numer" c_numer :: Ptr CanonicalForm -> IO (Ptr CanonicalForm)
foreign import ccall "denom" c_denom :: Ptr CanonicalForm -> IO (Ptr CanonicalForm)

getNumer :: CF -> IO CF
getNumer cf = withForeignPtr cf $ \ptr -> makeCF =<< (c_numer ptr)

getDenom :: CF -> IO CF
getDenom cf = withForeignPtr cf $ \ptr -> makeCF =<< (c_denom ptr)

foreign import ccall "index_poly" c_index_poly :: Ptr CanonicalForm -> Int -> IO (Ptr CanonicalForm)

getCfAtIndex :: CF -> Int -> IO CF
getCfAtIndex cf idx = withForeignPtr cf $ \ptr -> makeCF =<< (c_index_poly ptr $ fromIntegral idx)

foreign import ccall "plus"  c_plus  :: Ptr CanonicalForm -> Ptr CanonicalForm -> IO (Ptr CanonicalForm)
foreign import ccall "minus" c_minus :: Ptr CanonicalForm -> Ptr CanonicalForm -> IO (Ptr CanonicalForm)
foreign import ccall "times" c_times :: Ptr CanonicalForm -> Ptr CanonicalForm -> IO (Ptr CanonicalForm)

plusIO :: CF -> CF -> IO CF
plusIO x y = withForeignPtr x $ \p -> withForeignPtr y $ \q -> makeCF =<< (c_plus p q)

minusIO :: CF -> CF -> IO CF
minusIO x y = withForeignPtr x $ \p -> withForeignPtr y $ \q -> makeCF =<< (c_minus p q)

timesIO :: CF -> CF -> IO CF
timesIO x y = withForeignPtr x $ \p -> withForeignPtr y $ \q -> makeCF =<< (c_times p q)

--------------------------------------------------------------------------------

test = do
  cf <- newEmptyCF
  print =<< isZero cf
  print =<< isOne  cf
  cf <- newSmallConstCF 1
  print =<< isZero cf
  print =<< isOne  cf
  cf <- newSmallConstCF 2
  print =<< isZero cf
  print =<< isOne  cf

