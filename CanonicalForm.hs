
-- | Low-level bindings to singular-factory

{-# LANGUAGE BangPatterns, EmptyDataDecls, ForeignFunctionInterface #-}
module CanonicalForm where

--------------------------------------------------------------------------------

import Data.Word
import Data.Ratio

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array

import Numeric.GMP.Types
import qualified Numeric.GMP.Utils as GMP 

--------------------------------------------------------------------------------
-- * types

data Variable
data CanonicalForm
data Factor               -- CFFactor
data FactorList           -- List<CFFactor>

type Var     = ForeignPtr Variable
type CF      = ForeignPtr CanonicalForm
type Fac     = ForeignPtr Factor
type FacList = ForeignPtr FactorList

--------------------------------------------------------------------------------
-- * constants

foreign import ccall "level_trans" c_level_trans :: CInt  
foreign import ccall "level_base"  c_level_base  :: CInt  
foreign import ccall "level_quot"  c_level_quot  :: CInt  
foreign import ccall "level_expr"  c_level_expr  :: CInt  

--------------------------------------------------------------------------------
-- * memory management

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
-- * variables

foreign import ccall "new_var_level"      c_new_var_level      :: CInt  -> IO (Ptr Variable)
foreign import ccall "new_var_name"       c_new_var_name       :: CChar -> IO (Ptr Variable)
foreign import ccall "new_var_level_name" c_new_var_level_name :: CInt -> CChar -> IO (Ptr Variable)

newVarL :: Int -> IO Var
newVarL level = makeVar =<< c_new_var_level (fromIntegral level)

newVarN :: Char -> IO Var
newVarN name = makeVar =<< c_new_var_name (castCharToCChar name)

newVarLN :: Int -> Char -> IO Var
newVarLN level name = makeVar =<< c_new_var_level_name (fromIntegral level) (castCharToCChar name)

newTransVar :: IO Var
newTransVar = makeVar =<< c_new_var_level (c_level_trans)

foreign import ccall "root_of" c_root_of :: Ptr CanonicalForm -> IO (Ptr Variable)

newRootOf :: CF -> IO Var
newRootOf cf = withForeignPtr cf $ \ptr -> makeVar =<< c_root_of ptr

foreign import ccall "get_var_level" c_get_var_level :: Ptr Variable -> IO CInt
foreign import ccall "get_var_name"  c_get_var_name  :: Ptr Variable -> IO CChar

getVarLevel :: Var -> IO Int
getVarLevel var = withForeignPtr var $ \ptr -> fromIntegral <$> (c_get_var_level ptr)

getVarName :: Var -> IO Char
getVarName var = withForeignPtr var $ \ptr -> castCCharToChar <$> (c_get_var_name ptr)

foreign import ccall "has_mipo" c_has_mipo :: Ptr Variable -> IO CInt
foreign import ccall "get_mipo" c_get_mipo :: Ptr Variable -> Ptr Variable -> IO (Ptr CanonicalForm)
foreign import ccall "set_mipo" c_set_mipo :: Ptr Variable -> Ptr CanonicalForm -> IO ()
foreign import ccall "set_reduce" c_set_reduce :: Ptr Variable -> CInt -> IO ()

hasMinimalPoly :: Var -> IO Bool
hasMinimalPoly var = withForeignPtr var $ \ptr -> liftBool (c_has_mipo ptr)

getMinimalPoly  :: Var -> Var -> IO CF
getMinimalPoly var1 var2 = 
  withForeignPtr var1 $ \ptr1 -> 
    withForeignPtr var2 $ \ptr2 -> 
      makeCF =<< c_get_mipo ptr1 ptr2

setReduceFlag :: Var -> Bool -> IO ()
setReduceFlag var flag = withForeignPtr var $ \ptr -> c_set_reduce ptr (if flag then 1 else 0)

--------------------------------------------------------------------------------
-- * factors

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
-- * lists

foreign import ccall "get_list_length" c_get_list_length :: Ptr FactorList -> IO CInt

getListLength :: FacList -> IO Int
getListLength faclist = withForeignPtr faclist $ \ptr -> fromIntegral <$> c_get_list_length ptr

--------------------------------------------------------------------------------
-- * factorization

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

factorizeIO' :: CF -> IO FacList
factorizeIO' cf = withForeignPtr cf $ \ptr -> makeFacList (c_factorize ptr)

factorizeIO :: CF -> IO [(CF,Int)]
factorizeIO cf = unpackFactorList =<< factorizeIO' cf

--------------------------------------------------------------------------------
-- * basic CFs

foreign import ccall "empty_cf" c_empty_cf :: IO (Ptr CanonicalForm)
 
newEmptyCF :: IO CF
newEmptyCF = makeCF =<< c_empty_cf

foreign import ccall "const_cf" c_const_cf :: CInt -> IO (Ptr CanonicalForm)
 
newSmallConstCF :: Int -> IO CF
newSmallConstCF k = makeCF =<< c_const_cf (fromIntegral k)

foreign import ccall "var_cf"     c_var_cf     :: Ptr Variable ->        IO (Ptr CanonicalForm)
foreign import ccall "var_pow_cf" c_var_pow_cf :: Ptr Variable -> Int -> IO (Ptr CanonicalForm)

varIO :: Var -> IO CF
varIO var = withForeignPtr var $ \vptr -> makeCF =<< c_var_cf vptr

varPowIO :: Var -> Int -> IO CF
varPowIO var expo = withForeignPtr var $ \vptr -> makeCF =<< c_var_pow_cf vptr expo

--------------------------------------------------------------------------------
-- * basic CF predicates

liftBool :: IO CInt -> IO Bool
liftBool action = action >>= \k -> return (k/=0)

foreign import ccall "is_zero" c_is_zero :: Ptr CanonicalForm -> IO CInt
foreign import ccall "is_one"  c_is_one  :: Ptr CanonicalForm -> IO CInt

isZeroIO :: CF -> IO Bool
isZeroIO cf = withForeignPtr cf $ \ptr -> liftBool (c_is_zero ptr)

isOneIO :: CF -> IO Bool
isOneIO cf = withForeignPtr cf $ \ptr -> liftBool (c_is_one ptr)

foreign import ccall "is_imm"        c_is_imm        :: Ptr CanonicalForm -> IO CInt
foreign import ccall "is_univariate" c_is_univariate :: Ptr CanonicalForm -> IO CInt

isImmediateIO :: CF -> IO Bool
isImmediateIO cf = withForeignPtr cf $ \ptr -> liftBool (c_is_imm ptr)

isUnivariateIO :: CF -> IO Bool
isUnivariateIO cf = withForeignPtr cf $ \ptr -> liftBool (c_is_univariate ptr)

foreign import ccall "in_ZZ" c_in_ZZ :: Ptr CanonicalForm -> IO CInt
foreign import ccall "in_QQ" c_in_QQ :: Ptr CanonicalForm -> IO CInt
foreign import ccall "in_GF" c_in_GF :: Ptr CanonicalForm -> IO CInt
foreign import ccall "in_FF" c_in_FF :: Ptr CanonicalForm -> IO CInt

isInZZ_IO :: CF -> IO Bool
isInZZ_IO cf = withForeignPtr cf $ \ptr -> liftBool (c_in_ZZ ptr)

isInQQ_IO :: CF -> IO Bool
isInQQ_IO cf = withForeignPtr cf $ \ptr -> liftBool (c_in_QQ ptr)

isInGF_IO :: CF -> IO Bool
isInGF_IO cf = withForeignPtr cf $ \ptr -> liftBool (c_in_GF ptr)

isInFF_IO :: CF -> IO Bool
isInFF_IO cf = withForeignPtr cf $ \ptr -> liftBool (c_in_FF ptr)

foreign import ccall "in_BaseDomain"  c_in_BaseDomain  :: Ptr CanonicalForm -> IO CInt
foreign import ccall "in_CoeffDomain" c_in_CoeffDomain :: Ptr CanonicalForm -> IO CInt
foreign import ccall "in_PolyDomain"  c_in_PolyDomain  :: Ptr CanonicalForm -> IO CInt
foreign import ccall "in_Extension"   c_in_Extension   :: Ptr CanonicalForm -> IO CInt
foreign import ccall "in_QuotDomain"  c_in_QuotDomain  :: Ptr CanonicalForm -> IO CInt

isInBaseDomainIO :: CF -> IO Bool
isInBaseDomainIO cf = withForeignPtr cf $ \ptr -> liftBool (c_in_BaseDomain ptr)

isInCoeffDomainIO :: CF -> IO Bool
isInCoeffDomainIO cf = withForeignPtr cf $ \ptr -> liftBool (c_in_CoeffDomain ptr)

isInPolyDomainIO :: CF -> IO Bool
isInPolyDomainIO cf = withForeignPtr cf $ \ptr -> liftBool (c_in_PolyDomain ptr)

isInExtensionIO :: CF -> IO Bool
isInExtensionIO cf = withForeignPtr cf $ \ptr -> liftBool (c_in_Extension ptr)

isInQuotDomainIO :: CF -> IO Bool
isInQuotDomainIO cf = withForeignPtr cf $ \ptr -> liftBool (c_in_QuotDomain ptr)

--------------------------------------------------------------------------------
-- * basic properties

foreign import ccall "degree_of"      c_degree_of      :: Ptr CanonicalForm -> IO CInt
foreign import ccall "level_of"       c_level_of       :: Ptr CanonicalForm -> IO CInt
foreign import ccall "mvar_of"        c_mvar_of        :: Ptr CanonicalForm -> IO (Ptr Variable)

getDegree :: CF -> IO Int
getDegree cf = withForeignPtr cf $ \ptr -> fromIntegral <$> (c_degree_of ptr)

getLevel :: CF -> IO Int
getLevel cf = withForeignPtr cf $ \ptr -> fromIntegral <$> (c_level_of ptr)

getMainVar :: CF -> IO Var
getMainVar cf = withForeignPtr cf $ \ptr -> makeVar =<< (c_mvar_of ptr)

--------------------------------------------------------------------------------
-- * small values

foreign import ccall "smallint_value" c_smallint_value :: Ptr CanonicalForm -> IO CLong   -- !!!

getSmallIntValue :: CF -> IO Int
getSmallIntValue cf = withForeignPtr cf $ \ptr -> fromIntegral <$> (c_smallint_value ptr)

foreign import ccall "numer" c_numer :: Ptr CanonicalForm -> IO (Ptr CanonicalForm)
foreign import ccall "denom" c_denom :: Ptr CanonicalForm -> IO (Ptr CanonicalForm)

getNumer :: CF -> IO CF
getNumer cf = withForeignPtr cf $ \ptr -> makeCF =<< (c_numer ptr)

getDenom :: CF -> IO CF
getDenom cf = withForeignPtr cf $ \ptr -> makeCF =<< (c_denom ptr)

foreign import ccall "index_poly" c_index_poly :: Ptr CanonicalForm -> Int -> IO (Ptr CanonicalForm)
foreign import ccall "map_into"   c_map_into   :: Ptr CanonicalForm -> IO (Ptr CanonicalForm)
foreign import ccall "substitute" c_substitute :: Ptr CanonicalForm -> Ptr Variable -> Ptr CanonicalForm -> IO (Ptr CanonicalForm)

--------------------------------------------------------------------------------
-- * Polynomial operations

-- | Get the given degree part
getCfAtIndex :: CF -> Int -> IO CF
getCfAtIndex cf idx = withForeignPtr cf $ \ptr -> makeCF =<< (c_index_poly ptr $ fromIntegral idx)

-- | Map into the current base domain
mapInto :: CF -> IO CF
mapInto cf = withForeignPtr cf $ \ptr -> makeCF =<< (c_map_into ptr)

-- | Map into the current base domain
substituteIO :: Var -> CF -> (CF -> IO CF)
substituteIO var what cf = 
  withForeignPtr var $ \pvar -> 
    withForeignPtr what $ \pwhat -> 
      withForeignPtr cf $ \pcf -> 
        makeCF =<< (c_substitute pcf pvar pwhat)

--------------------------------------------------------------------------------
-- * Binary operations

foreign import ccall "is_equal" c_is_equal :: Ptr CanonicalForm -> Ptr CanonicalForm -> IO CInt

isEqualIO :: CF -> CF -> IO Bool
isEqualIO x y = withForeignPtr x $ \p -> withForeignPtr y $ \q -> liftBool (c_is_equal p q)

foreign import ccall "plus_cf"  c_plus_cf  :: Ptr CanonicalForm -> Ptr CanonicalForm -> IO (Ptr CanonicalForm)
foreign import ccall "minus_cf" c_minus_cf :: Ptr CanonicalForm -> Ptr CanonicalForm -> IO (Ptr CanonicalForm)
foreign import ccall "times_cf" c_times_cf :: Ptr CanonicalForm -> Ptr CanonicalForm -> IO (Ptr CanonicalForm)
foreign import ccall "pow_cf"   c_pow_cf   :: Ptr CanonicalForm -> CInt              -> IO (Ptr CanonicalForm)
foreign import ccall "div_cf"   c_div_cf   :: Ptr CanonicalForm -> Ptr CanonicalForm -> IO (Ptr CanonicalForm)
foreign import ccall "mod_cf"   c_mod_cf   :: Ptr CanonicalForm -> Ptr CanonicalForm -> IO (Ptr CanonicalForm)

foreign import ccall "gcd_poly_cf" c_gcd_poly_cf   :: Ptr CanonicalForm -> Ptr CanonicalForm -> IO (Ptr CanonicalForm)
foreign import ccall "reduce_cf"   c_reduce_cf     :: Ptr CanonicalForm -> Ptr CanonicalForm -> IO (Ptr CanonicalForm)

plusIO :: CF -> CF -> IO CF
plusIO x y = withForeignPtr x $ \p -> withForeignPtr y $ \q -> makeCF =<< (c_plus_cf p q)

minusIO :: CF -> CF -> IO CF
minusIO x y = withForeignPtr x $ \p -> withForeignPtr y $ \q -> makeCF =<< (c_minus_cf p q)

timesIO :: CF -> CF -> IO CF
timesIO x y = withForeignPtr x $ \p -> withForeignPtr y $ \q -> makeCF =<< (c_times_cf p q)

powIO :: CF -> Int -> IO CF
powIO x n = withForeignPtr x $ \p -> makeCF =<< (c_pow_cf p $ fromIntegral n)

divIO :: CF -> CF -> IO CF
divIO x y = withForeignPtr x $ \p -> withForeignPtr y $ \q -> makeCF =<< (c_div_cf p q)

modIO :: CF -> CF -> IO CF
modIO x y = withForeignPtr x $ \p -> withForeignPtr y $ \q -> makeCF =<< (c_mod_cf p q)

gcdPolyIO :: CF -> CF -> IO CF
gcdPolyIO x y = withForeignPtr x $ \p -> withForeignPtr y $ \q -> makeCF =<< (c_gcd_poly_cf p q)

reduceIO :: CF -> CF -> IO CF
reduceIO x y = withForeignPtr x $ \p -> withForeignPtr y $ \q -> makeCF =<< (c_reduce_cf p q)

--------------------------------------------------------------------------------
-- * GMP compatibility layer

foreign import ccall "get_gmp_numerator"   c_get_gmp_numerator   :: Ptr CanonicalForm -> Ptr MPZ -> IO ()
foreign import ccall "get_gmp_denominator" c_get_gmp_denominator :: Ptr CanonicalForm -> Ptr MPZ -> IO ()

getGmpNumerator :: CF -> IO Integer
getGmpNumerator cf =
  withForeignPtr cf $ \cfp -> 
    GMP.withOutInteger_ (\mpz_ptr -> c_get_gmp_numerator cfp mpz_ptr)

getGmpDenominator :: CF -> IO Integer
getGmpDenominator cf =
  withForeignPtr cf $ \cfp -> 
    GMP.withOutInteger_ (\mpz_ptr -> c_get_gmp_denominator cfp mpz_ptr)

foreign import ccall "make_ZZ_from_gmp" c_make_ZZ_from_gmp  :: Ptr MPZ -> IO (Ptr CanonicalForm)
foreign import ccall "make_QQ_from_gmp" c_make_QQ_from_gmp  :: Ptr MPZ -> Ptr MPZ -> CInt -> IO (Ptr CanonicalForm)

makeIntegerCF :: Integer -> IO CF
makeIntegerCF n = 
  GMP.withInInteger n $ \mpz_ptr -> 
    makeCF =<< (c_make_ZZ_from_gmp mpz_ptr)

makeRationalCF :: Rational -> IO CF
makeRationalCF n = 
  GMP.withInInteger (numerator n) $ \mpz_ptr1 -> 
    GMP.withInInteger (denominator n) $ \mpz_ptr2 ->  
      makeCF =<< (c_make_QQ_from_gmp mpz_ptr1 mpz_ptr2 0)

--------------------------------------------------------------------------------
-- * Base domain characteristic

foreign import ccall "get_characteristic"  c_get_characteristic  :: IO CInt
foreign import ccall "set_characteristic1" c_set_characteristic1 :: CInt -> IO ()
-- foreign import ccall "set_characteristic2" c_set_characteristic2 :: CInt -> CInt -> IO ()
foreign import ccall "set_characteristic3" c_set_characteristic3 :: CInt -> CInt -> CChar -> IO ()

getCharacteristic :: IO Int
getCharacteristic = fromIntegral <$> c_get_characteristic

-- | prime fields and QQ
setCharacteristic1 :: Int -> IO ()
setCharacteristic1 = c_set_characteristic1 . fromIntegral

{-
-- | prime power fields (meaning FF?)
-- apparently they removed this ????
setCharacteristic2 :: Int -> Int -> IO ()
setCharacteristic2 p n = c_set_characteristic2 (fromIntegral p) (fromIntegral n)
-}

-- | Galois fields
setCharacteristic3 :: Int -> Int -> Char -> IO ()
setCharacteristic3 p n ch = c_set_characteristic3 (fromIntegral p) (fromIntegral n) (castCharToCChar ch)

foreign import ccall "get_gf_value"     c_get_gf_value     :: Ptr CanonicalForm -> IO CInt
foreign import ccall "is_FF_in_GF"      c_is_FF_in_GF      :: Ptr CanonicalForm -> IO CInt
foreign import ccall "get_GF_degree"    c_get_GF_degree    :: IO CInt
foreign import ccall "get_GF_generator" c_get_GF_generator :: IO (Ptr CanonicalForm)

-- | This returns the exponent of the canonical generator.
-- If the input is zero, it appears to return the order of the field, q, but don't rely on this...
getGFValue :: CF -> IO Int
getGFValue cf = withForeignPtr cf $ \ptr -> fromIntegral <$> (c_get_gf_value ptr)

-- | True if element of the prime subfield
isFFinGF_IO :: CF -> IO Bool
isFFinGF_IO cf = withForeignPtr cf $ \ptr -> liftBool (c_is_FF_in_GF ptr)

-- | Returns the degree of the Galois field (degree of extension over the prime field)
getGFDegree :: IO Int
getGFDegree = fromIntegral <$> c_get_GF_degree

-- | Returns the generator of the Galois field
getGFGenerator :: IO CF
getGFGenerator = makeCF =<< c_get_GF_generator

--------------------------------------------------------------------------------

