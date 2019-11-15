
-- | Parsing polynomials and polynomial expressions

module Parser where

--------------------------------------------------------------------------------

import Data.Char
import Data.List

import Control.Applicative
import Control.Monad

import Data.Text.Lazy ( Text )
import qualified Data.Text.Lazy      as T
import qualified Data.Text.Lazy.Read as T

import DList as DList

--------------------------------------------------------------------------------

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

type Var = String

data Sign 
  = Plus 
  | Minus 
  deriving (Eq,Ord,Show)

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

_variableP :: Parser Var
_variableP = do
  x  <- charP          isAlpha
  xs <- charsP $ \c -> isAlpha c || isDigit c || (c == '_')
  return (x:xs)

varP :: Parser Var
varP = withSpaces _variableP

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

monomP :: Parser [(Var,Int)]
monomP = do
  ve <- varPowP
  mb <- try $ do
    charP_ (=='*')
    spacesP_
    ves <- monomP
    return ves
  case mb of
    Nothing  -> return (ve:[] )
    Just ves -> return (ve:ves)
  
--------------------------------------------------------------------------------

-- | Polynomial expressions
data Expr
  = VarE !Var
  | KstE !Integer
  | NegE Expr
  | LinE [(Sign,Expr)]
  | MulE [Expr]
  | PowE Expr !Int
  deriving (Eq,Ord,Show)

evalSign :: Num c => Sign -> c -> c
evalSign Plus  = id
evalSign Minus = negate
 
evalExpr :: Num c => (Var -> c) -> Expr -> c
evalExpr evalVar = go where
  go expr = case expr of
    VarE v   -> evalVar v
    KstE k   -> fromInteger k
    NegE e   -> negate (go e)
    LinE xs  -> sum [ evalSign pm (go x) | (pm,x) <- xs ]
    MulE xs  -> product (map go xs)
    PowE e k -> (go e)^k
    
--------------------------------------------------------------------------------

{-

prettExpr :: Expr -> String
prettExpr expr = DList.toList (prettPrecExpr 0 expr)

prettyPrecExpr :: Int -> Expr -> DList Char
prettyPrecExpr = go where

  chr c = DList.singleton c
  str s = DList.append s

  go !prec !expr = case expr of
    VarE !Var
    KstE !Integer
    NegE Expr
    LinE [(Sign,Expr)]
    MulE [Expr]
    PowE Expr !Int

-}

--------------------------------------------------------------------------------

exprP = do
  spacesP_ 
  level3

atomicP = (VarE <$> varP) <||> (KstE <$> kstP) <||> inParensP exprP

level0 =                                   atomicP
level1 =                         powP <||> atomicP 
level2 =           productP <||> powP <||> atomicP 
level3 = sumP <||> productP <||> powP <||> atomicP 

powP :: Parser Expr
powP = do
  (e,n) <- _powP
  return $ case n of
    0 -> KstE 1
    1 -> e
    _ -> PowE e n

_powP :: Parser (Expr,Int)
_powP = do
  e <- level0
  spacesP_
  charP_ (=='^')
  spacesP_
  n <- natP
  spacesP_
  return (e,n)

productP :: Parser Expr
productP = do
  es <- _productP
  return $ case es of
    [x] -> x
    _   -> MulE es

_productP :: Parser [Expr]
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

sumP :: Parser Expr
sumP = do
  es <- _sumP
  return $ case es of
    [(Plus ,x)] -> x
    [(Minus,x)] -> NegE x
    _           -> LinE es

_sumP :: Parser [(Sign,Expr)]
_sumP = do
  pm <- optionalSignP
  (this,rest) <- __sumP
  return ((pm,this):rest)

__sumP :: Parser (Expr,[(Sign,Expr)])
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

parseExpr :: Text -> Either String Expr
parseExpr text = case runParser (withEof exprP) text of
  Right (y,_) -> Right y
  Left  msg   -> Left msg

    