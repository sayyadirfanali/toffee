module Stmnt where

import Text.Megaparsec
import Text.Megaparsec.Debug

import Lexer
import Parser
import qualified Expr as E

data Stmnt 
  = SExpr   E.Expr
  | SBlock  [Stmnt]
  | SDecl   E.Expr E.Expr
  | SIf     E.Expr Stmnt (Maybe Stmnt)
  deriving (Eq, Show)

expr :: Parser Stmnt
expr = do
  e <- E.expr
  return (SExpr e)

decl :: Parser Stmnt
decl = do
  keyP "let"
  e <- E.expr
  symP "="
  f <- E.expr
  return (SDecl e f)

block :: Parser Stmnt
block = do
  keyP "do"
  let open = bracketP '<' <|> bracketP '{'
  b <- open
  xs <- sepEndBy stmnt semicolonP
  let close = if b == TBracket '<' then bracketP '>' else bracketP '}'
  close
  return (SBlock xs)

stmnt :: Parser Stmnt
stmnt = choice (fmap (uncurry dbg) $ ps)
  where
  ps = [
      ("block", block)
    , ("decl", decl)
    , ("expr", expr)
    ]
