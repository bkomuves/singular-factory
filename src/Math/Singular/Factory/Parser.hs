
-- | Parsing polynomials and polynomial expressions

{-# LANGUAGE BangPatterns #-}
module Math.Singular.Factory.Parser 
  ( -- * Parsing polynomials 
    parseExpr , parseGenPoly
  , parseStringExpr , parseStringGenPoly
    -- * Parser monad
  , Parser, runParser
  , (<||>) , try
  , many , many1
    -- * Parsers
  , charP , charP_ , charsP
  , spacesP_ , eofP  
  , signP , natP , integerP , identifierP
  , exprP , genPolyP
  ) 
  where

--------------------------------------------------------------------------------

import Data.Char
import Data.List

import Control.Applicative
import Control.Monad

import Data.Traversable

import Data.Proxy

import Data.Text.Lazy ( Text )
import qualified Data.Text.Lazy      as T
import qualified Data.Text.Lazy.Read as T

import Math.Singular.Factory.Expr
import Math.Singular.Factory.Variables

import Math.Singular.Factory.Internal.DList as DList

--------------------------------------------------------------------------------

type Var = String

--------------------------------------------------------------------------------
-- * the Parser monad

newtype Parser a = P { runParser :: Text -> Either String (a,Text) }

instance Functor Parser where
  fmap f (P action) = P $ \text -> case action text of
    Right (x, rest)   -> Right (f x, rest)
    Left  msg         -> Left msg

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  return x          = P $ \text -> Right (x,text)
  (P action) >>= u  = P $ \text -> case action text of
    Right (x, rest)   -> runParser (u x) rest
    Left  msg         -> Left msg

--------------------------------------------------------------------------------

try :: Parser a -> Parser (Maybe a)
try p = P $ \text -> case runParser p text of
  Right (x, rest) -> Right (Just x , rest)
  Left  msg       -> Right (Nothing, text)

infixr 5 <||>

(<||>) :: Parser a -> Parser a -> Parser a
(<||>) p q = do
  mb <- try p
  case mb of
    Just y  -> return y
    Nothing -> q

instance Alternative Parser where 
  (<|>) = (<||>)
 
many1 :: Parser a -> Parser [a]
many1 p = do
  x  <- p
  xs <- many p
  return (x:xs)

--------------------------------------------------------------------------------
  
charP_ :: (Char -> Bool) -> Parser ()
charP_ cond = void $ charP cond

charP :: (Char -> Bool) -> Parser Char
charP cond = P $ \text -> case T.uncons text of
  Nothing        -> Left "unexpected end of input"
  Just (ch,rest) -> if cond ch
    then Right (ch , rest)
    else Left "unexpected character"

charsP :: (Char -> Bool) -> Parser [Char]
charsP cond = P $ \text -> case T.span cond text of
  (this,rest) -> Right (T.unpack this, rest)

eofP :: Parser ()
eofP = P $ \text -> if T.null text then Right ((),text) else Left "expected the end of input"

spacesP_ :: Parser ()
spacesP_ = P $ \text -> case T.span isSpace text of 
  (_,rest) -> Right ((),rest) 

withEof :: Parser a -> Parser a 
withEof action = do
  y <- action
  spacesP_
  eofP
  return y

withSpaces :: Parser a -> Parser a 
withSpaces action = do
  y <- action
  spacesP_
  return y

--------------------------------------------------------------------------------
-- * Parsing simple things

_signP :: Parser Sign
_signP = charP (\c -> (c=='+' || c=='-')) >>= \ch -> return $ if (ch=='+') then Plus else Minus

signP :: Parser Sign
signP = withSpaces _signP

optionalSignP :: Parser Sign
optionalSignP = do
  mb <- try signP
  return $ case mb of
    Just pm -> pm
    Nothing -> Plus

inParensP :: Parser a -> Parser a
inParensP action = do
  charP_ (=='(')
  spacesP_
  y <- withSpaces action
  charP_ (==')')
  spacesP_
  return y

natP :: Parser Int
natP = P (T.decimal)

naturalP :: Parser Integer
naturalP = P (T.decimal)

integerP :: Parser Integer
integerP = P (T.signed T.decimal)

identifierP :: Parser String
identifierP = do
  x  <- charP          isAlpha
  xs <- charsP $ \c -> isAlpha c || isDigit c || (c == '_')
  return (x:xs)

--------------------------------------------------------------------------------
-- * parsing polynomials

varP :: Parser Var
varP = withSpaces identifierP

kstP :: Parser Integer
kstP = withSpaces integerP

varPowP :: Parser (Var,Int)
varPowP = do
  v <- varP
  mb <- try $ do
    charP_ (=='^')
    spacesP_
    expo <- natP
    spacesP_
    return expo
  case mb of
    Nothing -> return (v,1)
    Just e  -> return (v,e)

_monomP :: Parser [(Var,Int)]
_monomP = do
  ve <- varPowP
  mb <- try $ do
    charP_ (=='*')
    spacesP_
    ves <- _monomP
    return ves
  case mb of
    Nothing  -> return (ve:[] )
    Just ves -> return (ve:ves)

monomP :: Parser (Monom Var)
monomP = Monom <$> _monomP
  
_termP :: Parser (Integer, Monom Var)
_termP = do
  pm   <- withSpaces optionalSignP
  mbcf <- try (withSpaces integerP)
  void $ try $ withSpaces (charP_ (=='*'))
  monom <- case mbcf of
    Nothing -> monomP
    Just _  -> maybe (Monom []) id <$> try monomP
  let cf = maybe 1 id mbcf
  return (negateIfMinus pm cf , monom)
   
termP :: Parser (Term Integer Var)
termP = (uncurry Term) <$> _termP

genPolyP :: Parser (GenPoly Integer Var)
genPolyP = (GenPoly . filter isNotZero) <$> (spacesP_ >> many1 termP) where
  isNotZero (Term cf _) = cf /= 0
  
--------------------------------------------------------------------------------
-- * Parsing polynomial expressions

exprP :: Parser (Expr Var)
exprP = do
  spacesP_ 
  level3

atomicP = (VarE <$> varP) <||> (KstE <$> kstP) <||> inParensP exprP

level0 =                                   atomicP
level1 =                         powP <||> atomicP 
level2 =           productP <||> powP <||> atomicP 
level3 = sumP <||> productP <||> powP <||> atomicP 

powP :: Parser (Expr Var)
powP = do
  (e,n) <- _powP
  return $ case n of
    0 -> KstE 1
    1 -> e
    _ -> PowE e n

_powP :: Parser (Expr Var,Int)
_powP = do
  e <- level0
  spacesP_
  charP_ (=='^')
  spacesP_
  n <- natP
  spacesP_
  return (e,n)

productP :: Parser (Expr Var)
productP = do
  es <- _productP
  return $ case es of
    [x] -> x
    _   -> MulE es

_productP :: Parser [Expr Var]
_productP = do
  e1 <- level1
  spacesP_
  mb <- try $ do
    charP_ (=='*')
    spacesP_
    es <- _productP
    return es
  case mb of
    Nothing  -> return (e1:[])
    Just es  -> return (e1:es)

sumP :: Parser (Expr Var)
sumP = do
  es <- _sumP
  return $ case es of
    [(Plus ,x)] -> x
    [(Minus,x)] -> NegE x
    _           -> LinE es

_sumP :: Parser [(Sign,Expr Var)]
_sumP = do
  pm <- optionalSignP
  (this,rest) <- __sumP
  return ((pm,this):rest)

__sumP :: Parser ( Expr Var , [(Sign,Expr Var)] )
__sumP = do
  e1  <- level2
  spacesP_
  mb <- try $ do
    pm <- signP
    spacesP_
    (e,lin) <- __sumP
    return ((pm,e):lin)
  case mb of
    Nothing  -> return (e1,[] )
    Just lin -> return (e1,lin)

--------------------------------------------------------------------------------

parseStringExpr :: Text -> Either String (Expr Var)
parseStringExpr text = case runParser (withEof exprP) text of
  Right (y,_) -> Right y
  Left  msg   -> Left msg

parseStringGenPoly :: Text -> Either String (GenPoly Integer Var)
parseStringGenPoly text = case runParser (withEof genPolyP) text of
  Right (y,_) -> Right y
  Left  msg   -> Left msg

--------------------------------------------------------------------------------

parseGenPoly :: forall vars. VariableSet vars => Proxy vars -> Text -> Maybe (GenPoly Integer VarIdx)
parseGenPoly pxy text = case parseStringGenPoly text of
  Left {}  -> Nothing
  Right gp -> sequence $ fmap (recogVarName pxy) gp  

parseExpr :: forall vars. VariableSet vars => Proxy vars -> Text -> Maybe (Expr VarIdx)
parseExpr pxy text = case parseStringExpr text of
  Left {} -> Nothing
  Right e -> sequence $ fmap (recogVarName pxy) e 
  
--------------------------------------------------------------------------------
