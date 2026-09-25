module Expr where

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Debug

import Utils
import Lexer
import Parser
import Data.Function ((&))
import Data.Maybe (isJust)

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

data ArrElem = AESpread Expr | AEPlain Expr
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
  | ELam        [(Ident, Maybe TExpr)] Expr

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

  | ETypeDecl   Ident [Ident] TExpr
  deriving (Eq, Ord, Show)

-- name = identifier | key
-- member fields and object keys can be both
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
  args <- parenP $ sepBy argP commaP
  symP "=>"
  body <- exprE
  return $ ELam args body
  where
    argP = (,) <$> identP <*> (optional $ symP ":" *> texprE)

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

typeDeclE :: Parser Expr
typeDeclE = label "type declaration" $ do
  keyP "type"
  name <- identP
  params <- option [] (parenP $ sepBy identP commaP)
  symP "="
  ETypeDecl name params <$> texprE

exprE = choice [ lamE, ifthenE, blockE, declE, opE ]

data TUnOp
  = TReadOnly
  deriving (Eq, Ord, Show, Enum, Bounded)

data TBinOp
  = TUnion
  | TInter
  deriving (Eq, Ord, Show, Enum, Bounded)

data TTupElem
  = TTEPlain TExpr
  | TTEKeyVal Ident TExpr
  | TTEKeyValOpt Ident TExpr
  | TTESpread TExpr
  deriving (Eq, Ord, Show)

data TObjField
  = TOFKeyVal Ident Bool TExpr
  deriving (Eq, Ord, Show)

data TFuncParam
  = TFPIdTyp Ident Bool TExpr
  deriving (Eq, Ord, Show)

newtype Raw = Raw String
  deriving (Eq, Ord, Show)

data TExpr
  = TELit Lit
  | TEApp [Ident] [TExpr]
  | TEArr TExpr -- T[]
  | TETuple [TTupElem]
  | TEUnOp TUnOp TExpr
  | TEBinOp TBinOp TExpr TExpr
  | TEObj [TObjField]
  | TEFunc [Ident] [TFuncParam] TExpr
  | TECond TExpr TExpr TExpr
  | TERaw Raw
  deriving (Eq, Ord, Show)

tlitE :: Parser TExpr
tlitE = label "type literal" $ TELit <$> litP

tkwE :: Parser TExpr
tkwE = label "type keyword" $ (\k -> TEApp [Ident k] []) <$> keyP "void"

tarrE :: Parser (TExpr -> TExpr)
tarrE = label "type array" $ TEArr <$ (bracketP '[' *> bracketP ']')

trefE :: Parser TExpr
trefE = label "type name" $ do
  name <- sepBy1 identP (symP ".")
  args <- option [] (parenP $ sepBy texprE commaP)
  return $ TEApp name args

ttupE :: Parser TExpr
ttupE = label "type tuple" $
  TETuple <$> squareP (sepBy (TTEPlain <$> texprE) commaP)

tobjE :: Parser TExpr
tobjE = label "type object" $
  TEObj <$> curlyP (sepBy field commaP)
  where
    field = TOFKeyVal <$> (Ident <$> nameP) <*> (isJust <$> optional (symP "?")) <* symP ":" <*> texprE

tforallE :: Parser TExpr
tforallE = label "forall" $ do
  keyP "forall"
  vs <- some identP
  symP "."
  tfuncE vs

tfuncE :: [Ident] -> Parser TExpr
tfuncE vs = label "function type" $ do
  keyP "fn"
  ps  <- parenP $ sepBy param commaP
  symP "=>"
  ret <- texprE
  return (TEFunc vs ps ret)
  where
    param = TFPIdTyp <$> identP <*> ((isJust <$> optional (symP "?"))) <* symP ":" <*> texprE

trawE :: Parser TExpr
trawE = label "raw type" $ (TERaw . Raw) <$> (keyP "raw" *> stringP)

tprimE :: Parser TExpr
tprimE = choice [ trefE, tkwE, tlitE, ttupE, tobjE, trawE, parenP texprE ]

tpostE :: Parser TExpr
tpostE = do
  prim  <- tprimE
  postL <- many tarrE
  return $ foldl (&) prim postL

topE :: Parser TExpr
topE = makeExprParser tpostE table
  where
    binop op s = InfixL (TEBinOp op <$ symP s)
    table =
      [ [ binop TInter "&" ]
      , [ binop TUnion "|" ]
      ]
texprE :: Parser TExpr
texprE = choice [ tforallE, tfuncE [], topE ]