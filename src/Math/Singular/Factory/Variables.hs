
-- | Variables.
--
-- Singular-factory supports only a single linear sequence of variables,
-- indexed starting from 1, optionally having single-character names.
--
-- So what we do is to use (phantom) types to encode naming conventions.
--

{-# LANGUAGE BangPatterns, DataKinds, ScopedTypeVariables, KindSignatures #-}
module Math.Singular.Factory.Variables where

--------------------------------------------------------------------------------

import Data.Char
import Data.List ( findIndex )
import Text.Read

import Data.Proxy
import GHC.TypeLits

import System.IO.Unsafe as Unsafe

import Math.Singular.Factory.Internal.CanonicalForm -- Factory

--------------------------------------------------------------------------------
-- * Raw factory variables

-- | A variable index. 
--
-- In factory, there is a single linear sequence of variables.
-- Variables are indexed starting from 1.
-- 
type VarIdx = Int

-- | In factory, there is a single linear sequence of variables.
-- We \"precalculate\" these (lazily).
theFactoryVars :: [Var]
theFactoryVars = map mk [1..] where
  mk i = Unsafe.unsafePerformIO $ newVarL i

theNthVar :: VarIdx -> Var
theNthVar idx = theFactoryVars !! (idx-1)

--------------------------------------------------------------------------------
-- * Variable sets    

-- | The class of variable sets. Since Factory only supports a single linear
-- variable set, these differ only by naming conventions.
--
class VariableSet v where
  varIdxName   :: Proxy v -> VarIdx -> String
  recogVarName :: Proxy v -> String -> Maybe VarIdx

-- | The variable set @x1, x2, x3, x4...@ (where \"x\" can be any string)
data VarN  (s :: Symbol)   

-- | The variable set @x_1, x_2, x_3, x_4...@ (where \"x\" can be any string)
data Var_N (s :: Symbol)           

-- | The variable set @x[1], x[2], x[3], x[4]...@ (where \"x\" can be any string)
data VarBracketN (s :: Symbol)     

-- | The variable set @a, b, c, d...@
data VarAbc    

-- | The variable set @A, B, C, D...@
data VarABC    

-- | The variable set @x, y, z, u, v, w, a, b, c...@
data VarXyz    

-- | The variable set @X, Y, Z, U, V, W, A, B, C...@
data VarXYZ    

instance forall s. KnownSymbol s => VariableSet (VarN s) where
  varIdxName   _ = indexedVars  (symbolVal (Proxy :: Proxy s))
  recogVarName _ = recogIndexed (symbolVal (Proxy :: Proxy s))

instance forall s. KnownSymbol s => VariableSet (Var_N s) where
  varIdxName   _ = indexedVarsUnderscore  (symbolVal (Proxy :: Proxy s))
  recogVarName _ = recogIndexedUnderscore (symbolVal (Proxy :: Proxy s))

instance forall s. KnownSymbol s => VariableSet (VarBracketN s) where
  varIdxName   _ = indexedVarsBracket  (symbolVal (Proxy :: Proxy s))
  recogVarName _ = recogIndexedBracket (symbolVal (Proxy :: Proxy s))

instance VariableSet VarAbc where
  varIdxName   _ = abcVars
  recogVarName _ = recogAbc  

instance VariableSet VarABC where
  varIdxName   _ = capitalAbcVars
  recogVarName _ = recogABC

instance VariableSet VarXyz where
  varIdxName   _ = xyzVars
  recogVarName _ = recogXyz
  
instance VariableSet VarXYZ where
  varIdxName   _ = capitalXyzVars
  recogVarName _ = recogXYZ
  
--------------------------------------------------------------------------------
-- * Standard naming conventions of variables

-- | Eg. @x1, x2, x3...@
indexedVars :: String -> VarIdx -> String
indexedVars prefix = \i -> prefix ++ show i

-- | Eg. @x_1, x_2, x_3...@
indexedVarsUnderscore :: String -> VarIdx -> String
indexedVarsUnderscore prefix = \i -> prefix ++ "_" ++ show i

-- | Eg. @x[1], x[2], x[3]...@
indexedVarsBracket :: String -> VarIdx -> String
indexedVarsBracket prefix = \i -> prefix ++ "[" ++ show i ++ "]"

-- | That is, @a, b, c...@
abcVars :: VarIdx -> String
abcVars idx = lowerVarList !! (idx-1)

-- | That is, @A, B, C...@
capitalAbcVars :: VarIdx -> String
capitalAbcVars idx = lowerVarList !! (idx-1)

-- | @x, y, z, u, v, w, a, b, c ... , t@
xyzVars :: VarIdx -> String
xyzVars idx = [ varListXyz !! (idx-1) ]

capitalXyzVars :: VarIdx -> String
capitalXyzVars idx = [ varListCapitalXYZ !! (idx-1) ]

--------------------------------------------------------------------------------
-- * Variable lists

varListXyz :: [Char]
varListXyz = "xyzuvwabcdefghijklmnopqrst" 

varListCapitalXYZ :: [Char]
varListCapitalXYZ = "XYZUVWABCDEFGHIJKLMNOPQRST"

-- | The infinite list of variables @a, b ..., z, aa, ab, ac, ...@
lowerVarList :: [String]
lowerVarList = map (:[]) abc ++ [ ys ++ [y] | ys<-lowerVarList , y<-abc ] where
  abc = ['a'..'z']

upperVarList :: [String]
upperVarList = map (:[]) abc ++ [ ys ++ [y] | ys<-lowerVarList , y<-abc ] where
  abc = ['A'..'Z']

--------------------------------------------------------------------------------
-- * Parsing standard variable names

readPosIdxMaybe :: String -> Maybe VarIdx
readPosIdxMaybe s = case readMaybe s :: Maybe Word of
  Nothing -> Nothing
  Just w  -> let j = (fromIntegral w :: Int) 
             in  if j >= 1 then Just j else Nothing
              
recogIndexed :: String -> String -> Maybe VarIdx
recogIndexed !prefix = recog where
  !n = length prefix
  recog s = case splitAt n s of 
    (p,q) -> if p /= prefix
      then Nothing
      else readPosIdxMaybe q 
      
recogIndexedUnderscore :: String -> String -> Maybe VarIdx
recogIndexedUnderscore !prefix = recog where
  !n = length prefix
  recog s = case splitAt n s of 
    (p,q) -> if p /= prefix
      then Nothing
      else case q of
        ('_':r ) -> readPosIdxMaybe r
        _        -> Nothing
        
recogIndexedBracket :: String -> String -> Maybe VarIdx
recogIndexedBracket !prefix = recog where
  !n = length prefix
  recog s = case splitAt n s of 
    (p,q) -> if p /= prefix
      then Nothing
      else if length q >= 3 && head q == '[' && last q == ']'
        then readPosIdxMaybe (init $ tail q) 
        else Nothing
     
recogAbc :: String -> Maybe VarIdx
recogAbc [c] = if c >= 'a' && c <= 'z' then Just (ord c - 96) else Nothing
recogAbc []  = Nothing
recogAbc _   = Nothing 

recogABC :: String -> Maybe VarIdx
recogABC [c] = if c >= 'A' && c <= 'Z' then Just (ord c - 64) else Nothing
recogABC []  = Nothing
recogABC _   = Nothing 

recogXyz :: String -> Maybe VarIdx
recogXyz [c] = if c >= 'a' && c <= 'z' then (+1) <$> findIndex (==c) varListXyz else Nothing
recogXyz _   = Nothing 

recogXYZ :: String -> Maybe VarIdx
recogXYZ [c] = if c >= 'A' && c <= 'Z' then (+1) <$> findIndex (==c) varListCapitalXYZ else Nothing
recogXYZ _   = Nothing 

--------------------------------------------------------------------------------
        