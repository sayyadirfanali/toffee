module Expr where

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Debug

import Utils
import Lexer
import Parser

data Expr
  = ELit    Lit
  | EIden   String
  | EArr    [Expr]
  | EObj    [(Expr, Expr)]
  | EMem    Expr Expr
  | ELam    [Expr] Expr
  | EApp    Expr [Expr]
  | EUnOp   String Expr
  | EBinOp  String Expr Expr
  deriving (Eq, Ord, Show)

lit :: Parser Expr
lit = do
  TLit x <- satisfyP (consP "Lit")
  return (ELit x)

string :: Parser Expr
string = do
  TLit s <- satisfyP isStr
  return (ELit s)
  where 
  isStr (TLit (LStr _)) = True
  isStr _               = False

iden :: Parser Expr
iden = do
  TIden s <- satisfyP (consP "Iden")
  return (EIden s)

arr :: Parser Expr
arr = do
  xs <- parenP (sepBy expr commaP)
  return (EArr xs)

obj :: Parser Expr
obj = do
  xs <- curlyP (sepBy keyval commaP)
  return (EObj xs)
  where
  keyval  = do
    k <- iden <|> string
    symP ":"
    v <- expr
    return (k, v)

lam :: Parser Expr
lam = do
  keyP "fn"
  args <- many iden
  symP "->"
  body <- expr
  return (ELam args body)

app :: Parser Expr
app = do
  x  <- try (p <* o)
  xs <- sepBy expr commaP <* c
  return (EApp x xs)
  where
  p = iden <|> parenP expr
  o = bracketP '('
  c = bracketP ')'

mem :: Parser Expr
mem = do
  x  <- try (p <* s)
  xs <- sepBy p s
  return (foldl EMem x xs)
  where
    p = app <|> iden <|> parenP expr
    s = symP "."

op :: Parser Expr
op = do
  let pre  = choice . fmap symP $ [ "+", "-" ]
      post = choice . fmap symP $ [ "+", "-", "*", "/", "<", ">" ]
  try . lookAhead $ (pre *> term) <|> (term <* post)
  makeExprParser term table
  where
  term     = choice [ iden, lit, parenP expr ]
  unop s   = symP s *> return (EUnOp s)
  binop s  = symP s *> return (EBinOp s)
  table    = [
        [ Postfix (unop "--"),  Postfix (unop "++") ]
      , [ Prefix  (unop "--"),  Prefix  (unop "++") ]
      , [ Prefix  (unop "-") ]
      , [ InfixL  (binop "*"),  InfixL (binop "/")  ]
      , [ InfixL  (binop "+"),  InfixL (binop "-")  ]
      , [
          InfixL  (binop "<"),  InfixL (binop ">"),
          InfixL  (binop "=<"), InfixL (binop ">=")
        ]
    ]

expr :: Parser Expr
expr = choice . fmap (uncurry dbg) $ ps
  where
  ps = [
        ("lam",  lam)
      , ("app",  app)
      , ("mem",  mem)
      , ("op",   op)
      , ("iden", iden)
      , ("arr",  arr)
      , ("obj",  obj)
      , ("lit",  lit)
    ]
