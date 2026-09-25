module Expr where

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Debug

import Utils
import Lexer
import Parser
import Data.Function ((&))

data UnOp = Neg | Pos | Not | TypeOf | Void | Await
  deriving (Eq, Ord, Show, Enum, Bounded)

unop2str :: UnOp -> String
unop2str op = case op of
  Neg    -> "-"
  Pos    -> "+"
  Not    -> "!"
  TypeOf -> "typeof"
  Void   -> "void"
  Await  -> "await"

data BinOp
  = Add | Sub | Mul | Div | Mod | Pow
  | Lt | Lte | Gt | Gte | InstanceOf
  | Eq | Neq
  | BAnd | BOr | BXor | Shl | Shr | Ushr
  | And | Or | Coalesce
  deriving (Eq, Ord, Show, Enum, Bounded)

binop2str :: BinOp -> String
binop2str op = case op of
  Add        -> "+"
  Sub        -> "-"
  Mul        -> "*"
  Div        -> "/"
  Mod        -> "%"
  Pow        -> "**"
  Lt         -> "<"
  Lte        -> "<="
  Gt         -> ">"
  Gte        -> ">="
  InstanceOf -> "instanceof"
  Eq         -> "=="
  Neq        -> "/="
  BAnd       -> "&"
  BOr        -> "|"
  BXor       -> "^"
  Shl        -> "<<"
  Shr        -> ">>"
  Ushr       -> ">>>"
  And        -> "&&"
  Or         -> "||"
  Coalesce   -> "??"

data ArrElem = AESpread Expr | AEPlain  Expr
  deriving (Eq, Ord, Show)

newtype ObjKey = ObjKey String
  deriving (Eq, Ord, Show)

data ObjField
  = OFSpread    Expr
  | OFKeyVal    ObjKey Expr
  | OFShort     Ident
  | OFComputed  Expr Expr
  deriving (Eq, Ord, Show)

newtype Ident = Ident String
  deriving (Eq, Ord, Show)

newtype MemName = MemName String
  deriving (Eq, Ord, Show)

data DeclTyp = LetDecl | ConstDecl
  deriving (Eq, Ord, Show)

data BlockTyp = Expl | Impl
  deriving (Eq, Ord, Show)

newtype BlockKey = BlockKey String
  deriving (Eq, Ord, Show)

data Expr
  = ELit        Lit
  | EIden       Ident

  | EArr        [ArrElem]
  | EObj        [ObjField]
  | ELam        [Ident] Expr

  | EMem        Expr MemName
  | EOptMem     Expr MemName

  | EApp        Expr [Expr]
  | EOptApp     Expr [Expr]

  | EIndex      Expr Expr
  | EOptIndex   Expr Expr

  | ENew        Expr [Expr]
  | ENonNull    Expr

  | EUnOp       UnOp Expr
  | EBinOp      BinOp Expr Expr

  | EIfThen     Expr Expr Expr

  | EDecl       DeclTyp Expr Expr
  | EBlock      BlockTyp BlockKey [Expr]
  deriving (Eq, Ord, Show)

-- identifier or key (needed for member fields and object keys)
nameP :: Parser String
nameP = matchP getName
  where
    getName (TIden s) = Just s
    getName (TKey s)  = Just s
    getName _         = Nothing

litP :: Parser Lit
litP = matchP getLit
  where
    getLit (TLit l) = Just l
    getLit _        = Nothing

litE :: Parser Expr
litE = label "literal" $
  ELit <$> litP

identP :: Parser Ident
identP = matchP getIden
  where
    getIden (TIden s) = Just (Ident s)
    getIden _         = Nothing

identE :: Parser Expr
identE = label "identifier" $ EIden <$> identP

arrE :: Parser Expr
arrE = label "array" $ 
  EArr <$> squareP (sepBy (spread <|> plain) commaP)
  where
    spread = AESpread <$> (symP "..." *> exprE)
    plain  = AEPlain <$> exprE

objE :: Parser Expr
objE = label "object" $
  EObj <$> curlyP (sepBy (keyVal <|> short <|> spread <|> computed) commaP)
  where
  key = ObjKey <$> (nameP <|> stringP)
  col  = symP ":"
  dots = symP "..."
  keyVal   = OFKeyVal   <$> (try $ key <* col) <*> exprE
  short    = OFShort    <$> (identP <* notFollowedBy col)
  spread   = OFSpread   <$> (dots *> exprE)
  computed = OFComputed <$> ((squareP exprE) <* col) <*> exprE

lamE :: Parser Expr
lamE = label "lambda" $ do
  keyP "fn"
  args <- parenP $ sepBy identP commaP
  symP "->"
  body <- exprE
  return $ ELam args body

ifthenE :: Parser Expr
ifthenE = label "if then" $ do
  keyP "if"
  cond <- exprE
  keyP "then"
  expThen <- exprE
  keyP "else"
  expElse <- exprE
  return $ EIfThen cond expThen expElse

memE :: Parser (Expr -> Expr)
memE = label "member" $ do
  symP "."
  m <- (MemName <$> nameP)
  return $ \x -> EMem x m

appE :: Parser (Expr -> Expr)
appE = label "application" $ flip EApp <$> (parenP $ sepBy exprE commaP)

indexE :: Parser (Expr -> Expr)
indexE = label "index" $ flip EIndex <$> (squareP exprE)

optPostE :: Parser (Expr -> Expr)
optPostE = label "optional" $ do
  symP "?."
  choice [
      flip EOptMem   <$> (MemName <$> nameP)
    , flip EOptApp   <$> (parenP $ sepBy exprE commaP)
    , flip EOptIndex <$> (squareP exprE)
    ]

nonNullE :: Parser (Expr -> Expr)
nonNullE = label "non null" $ do
  symP "!"
  return $ \x -> ENonNull x

postfixE :: Parser Expr
postfixE = do
  prim  <- choice [ identE, litE, arrE, objE, newE, parenP exprE ]
  postL <- many $ choice ([ optPostE, memE, appE, indexE, nonNullE ])
  return $ foldl (&) prim postL

newE = label "new" $
  keyP "new" *> (ENew <$> newPostFixE <*> (parenP $ sepBy exprE commaP))

-- `newPrim` produced same as `prim` in `postfixE` (minus `newE`)
-- `newPostL` avoids `appE` because `new Widget(x, y)` means `ENew Widget [x, y]` and not `ENew (EApp Widget [x, y]) []`
newPostFixE :: Parser Expr
newPostFixE = do
  newPrim  <- choice [ identE, litE, parenP exprE, arrE, objE ]
  newPostL <- many $ choice [ memE, indexE ]
  return $ foldl (&) newPrim newPostL

opE :: Parser Expr
opE = makeExprParser postfixE table
  where
    unop  op = EUnOp op <$ symP (unop2str op)
    kunop op = EUnOp op <$ keyP (unop2str op)
    binop op = EBinOp op <$ symP (binop2str op)
    kbin  op = EBinOp op <$ keyP (binop2str op)

    prefixop = Prefix $ foldr1 (.) <$> (some . choice $ [ unop Neg, unop Pos, unop Not ] <> [ kunop TypeOf, kunop Void, kunop Await ])

    table =
      [ [ prefixop ]
      , [ InfixR (binop Pow) ]
      , [ InfixL (binop Mul), InfixL (binop Div), InfixL (binop Mod) ]
      , [ InfixL (binop Add), InfixL (binop Sub) ]
      , [ InfixL (binop Shl), InfixL (binop Shr), InfixL (binop Ushr) ]
      , [ InfixN (binop Lt), InfixN (binop Lte), InfixN (binop Gt), InfixN (binop Gte)
        , InfixN (kbin InstanceOf) ]
      , [ InfixN (binop Eq), InfixN (binop Neq) ]
      , [ InfixL (binop BAnd) ]
      , [ InfixL (binop BXor) ]
      , [ InfixL (binop BOr) ]
      , [ InfixR (binop And) ]
      , [ InfixR (binop Or) ]
      , [ InfixR (binop Coalesce) ]
      ]

blockE :: Parser Expr
blockE = do
  k <- choice . fmap keyP $ [ "where", "do" ]
  b <- bracketP '<' <|> bracketP '{'
  xs <- sepEndBy exprE semicolonP
  if b == '<' then bracketP '>' else bracketP '}'
  return (EBlock (if b == '<' then Impl else Expl) (BlockKey k) xs)

declE :: Parser Expr
declE = do
  key <- keyP "let" <|> keyP "const"
  pat <- exprE -- remember to filter later (every expr is not a valid pattern)
  symP "="
  exp <- exprE
  return (EDecl (if key == "let" then LetDecl else ConstDecl) pat exp)

exprE :: Parser Expr
exprE = choice [ lamE, ifthenE, blockE, declE, opE ]