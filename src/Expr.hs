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
  Neq        -> "!="
  BAnd       -> "&"
  BOr        -> "|"
  BXor       -> "^"
  Shl        -> "<<"
  Shr        -> ">>"
  Ushr       -> ">>>"
  And        -> "&&"
  Or         -> "||"
  Coalesce   -> "??"

data AssignOp
  = AEq | AAdd | ASub | AMul | ADiv | AMod | APow
  | AShl | AShr | AUshr
  | ABoolAnd | ABoolOr | ABoolXor
  | AAnd | AOr | ACoalesce
  deriving (Eq, Ord, Show, Enum, Bounded)

assignop2str :: AssignOp -> String
assignop2str op = case op of
  AEq       -> "="
  AAdd      -> "+="
  ASub      -> "-="
  AMul      -> "*="
  ADiv      -> "/="
  AMod      -> "%="
  APow      -> "**="
  AShl      -> "<<="
  AShr      -> ">>="
  AUshr     -> ">>>="
  ABoolAnd  -> "&="
  ABoolOr   -> "|="
  ABoolXor  -> "^="
  AAnd      -> "&&="
  AOr       -> "||="
  ACoalesce -> "??="

-- `Ident` is any binding (variable, parameter, type) but never be a keyword (eg, `if`, `type`)
newtype Ident = Ident String
  deriving (Eq, Ord, Show)

-- `MemName` is whatever can follow member access markers `.` or `?.` (this includes keywords, eg, `window.type` is valid)
data MemName
  = MNPub  String
  | MNPriv String  -- `#xyz`
  deriving (Eq, Ord, Show)

-- `PropKey` is whatever name a property can take where it is *defined* (strictly larger than `MemName`)
data PropKey
  = PKMem  MemName -- see above
  | PKStr  String
  | PKNum  String
  | PKComp Expr -- [Symbol.iterator]
  deriving (Eq, Ord, Show)

data Elem
  = ElemPlain Expr
  | ElemSpread Expr -- `...x`
  deriving (Eq, Ord, Show)

data LamParam
  = LPPlain Ident (Maybe TExpr) (Maybe Expr)
  | LPRest  Ident (Maybe TExpr)
  deriving (Eq, Ord, Show)

data ObjField
  = OFSpread    Expr
  | OFKeyVal    PropKey Expr
  | OFShort     Ident
  | OFShortInit Ident Expr
  deriving (Eq, Ord, Show)

data MemMod
  = MMStatic
  | MMProtected
  | MMReadonly
  | MMAbstract
  | MMOverride
  deriving (Eq, Ord, Show, Enum, Bounded)

data ClassMem
  = CMField
  { cmMods  :: [MemMod]
  , cmProp  :: PropKey
  , cmIsOpt :: Bool
  , cmMType :: Maybe TExpr
  , cmMVal  :: Maybe Expr
  }
  | CMethod
  { cmMods    :: [MemMod]
  , cmProp    :: PropKey
  , cmFuncSig :: FuncSig
  , cmMBody   :: Maybe Expr
  }
  deriving (Eq, Ord, Show)

data ClassDecl = ClassDecl
  { cdAbstract :: Bool
  , cdName     :: Maybe Ident -- denotes anonymous class if `Nothing`
  , cdParams   :: [Ident]
  , cdExtends  :: Maybe TExpr
  , cdImpls    :: [TExpr]
  , cdMembers  :: [ClassMem]
  }
  deriving (Eq, Ord, Show)

data DeclTyp = LetDecl | ConstDecl
  deriving (Eq, Ord, Show)

data BlockTyp = Expl | Impl
  deriving (Eq, Ord, Show)

newtype BlockKey = BlockKey String
  deriving (Eq, Ord, Show)

data FuncSig = FuncSig
  { fsVars   :: [Ident]
  , fsParams :: [LamParam]
  , fsRet    :: Maybe TExpr
  }
  deriving (Eq, Ord, Show)

data Expr
  = ELit        Lit
  | EIden       Ident

  | EArr        [Elem]
  | EObj        [ObjField]
  | ELam        FuncSig Expr

  | EMem        Expr MemName
  | EOptMem     Expr MemName

  | EApp        Expr [Elem]
  | EOptApp     Expr [Elem]

  | EIndex      Expr Expr
  | EOptIndex   Expr Expr

  | ENew        Expr [Elem]
  | ENonNull    Expr

  | EUnOp       UnOp Expr
  | EBinOp      BinOp Expr Expr

  | EIfThen     Expr Expr Expr

  | EAssign     AssignOp Expr Expr

  | EClass      ClassDecl
  | EDecl       DeclTyp Expr (Maybe TExpr) Expr
  | EBlock      BlockTyp BlockKey [Expr]

  | ETypeDecl   Ident [Ident] TExpr
  deriving (Eq, Ord, Show)

colP :: Parser String
colP = symP ":"

dotsP :: Parser String
dotsP = symP "..."

fatP :: Parser String
fatP = symP "=>"

litP :: Parser Lit
litP = matchP $ \case
  TLit l -> Just l
  _      -> Nothing

numP :: Parser String
numP = matchP $ \case
  TLit (LNum s) -> Just s
  _             -> Nothing

identP :: Parser Ident
identP = matchP $ \case
  TIden s -> Just (Ident s)
  _       -> Nothing

privP :: Parser String
privP = matchP $ \case
  TPriv s -> Just s
  _       -> Nothing

nameWhereP :: (String -> Bool) -> Parser String
nameWhereP p = matchP $ \case
  TIden s | p s -> Just s
  TKey  s | p s -> Just s
  _            -> Nothing

nameP :: Parser String
nameP = nameWhereP (const True)

wordP :: String -> Parser ()
wordP s = label s (() <$ nameWhereP (== s))

-- `: T`, in params, fields, declarations
annotP :: Parser (Maybe TExpr)
annotP = optional (colP *> texprE)

-- `= e`, in params and fields
initP :: Parser (Maybe Expr)
initP = optional (symP "=" *> exprE)

-- `?`, in fields, type objects, function-type params
optFlagP :: Parser Bool
optFlagP = isJust <$> optional (symP "?")

-- `(A, B)`, in `type` and `class`
tparamsP :: Parser [Ident]
tparamsP = option [] (parenP (sepBy identP commaP))

-- `forall T U.`
forallP :: Parser [Ident]
forallP = option [] (keyP "forall" *> some identP <* symP ".")

-- `forall T U. fn`, in lambdas and methods
fnHeadP :: Parser [Ident]
fnHeadP = forallP <* keyP "fn"

-- layout block which can be implicit `< ... >` or explicit `{ ... }`
blockOf :: Parser a -> Parser (BlockTyp, [a])
blockOf p = choice
  [ (,) Impl <$> between (bracketP '<') (bracketP '>') items
  , (,) Expl <$> between (bracketP '{') (bracketP '}') items
  ]
  where
    items = sepEndBy p semicolonP

-- anything after `.` / `?.`
memNameP :: Parser MemName
memNameP = label "member name" $ (MNPub <$> nameP) <|> (MNPriv <$> privP)

nonShortHandKeyP :: Parser PropKey
nonShortHandKeyP = choice
  [ PKStr  <$> stringP
  , PKNum  <$> numP
  , PKComp <$> squareP exprE
  ]

-- object literal and type object keys
propKeyP :: Parser PropKey
propKeyP = label "property key" $ (PKMem . MNPub <$> nameP) <|> nonShortHandKeyP

litE :: Parser Expr
litE = label "literal" $ ELit <$> litP

identE :: Parser Expr
identE = label "identifier" $ EIden <$> identP

elemE :: Parser Elem
elemE = label "element" $ choice [ ElemSpread <$> (dotsP *> exprE), ElemPlain <$> exprE ]

argsP :: Parser [Elem]
argsP = parenP (sepEndBy elemE commaP)

arrE :: Parser Expr
arrE = label "array" $ EArr <$> squareP (sepEndBy elemE commaP)

objE :: Parser Expr
objE = label "object" $ EObj <$> curlyP (sepEndBy fieldP commaP)
  where
    fieldP  = choice [ spreadP, keyValP, shortP ]
    spreadP = OFSpread <$> (dotsP *> exprE)
    -- `OFKeyVal` needs try because it can be `OFShort` or `OFShortInit`
    keyValP = OFKeyVal
      <$> (try (PKMem . MNPub <$> nameP <* colP) <|> (nonShortHandKeyP <* colP))
      <*> exprE
    shortP = do
      i <- identP
      (OFShortInit i <$> (symP "=" *> exprE)) <|> return (OFShort i)

-- everything after `fn` (and after the method key, for methods)
sigTailP :: [Ident] -> Parser FuncSig
sigTailP vars = FuncSig vars <$> parenP (endByRest plainP restP) <*> annotP
  where
    plainP = LPPlain <$> identP <*> annotP <*> initP
    restP  = LPRest  <$> (dotsP *> identP) <*> annotP

funcsigP :: Parser FuncSig
funcsigP = fnHeadP >>= sigTailP

lamE :: Parser Expr
lamE = label "lambda" $ ELam <$> funcsigP <* fatP <*> exprE

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
  m <- memNameP
  return $ \x -> EMem x m

appE :: Parser (Expr -> Expr)
appE = label "application" $ flip EApp <$> argsP

indexE :: Parser (Expr -> Expr)
indexE = label "index" $ flip EIndex <$> squareP exprE

optPostE :: Parser (Expr -> Expr)
optPostE = label "optional" $ do
  symP "?."
  choice
    [ flip EOptMem   <$> memNameP
    , flip EOptApp   <$> argsP
    , flip EOptIndex <$> squareP exprE
    ]

nonNullE :: Parser (Expr -> Expr)
nonNullE = label "non null" $ ENonNull <$ symP "!"

postfixE :: Parser Expr
postfixE = do
  prim  <- choice [ identE, litE, arrE, objE, newE, parenP exprE ]
  postL <- many $ choice [ optPostE, memE, appE, indexE, nonNullE ]
  return $ foldl (&) prim postL

newE :: Parser Expr
newE = label "new" $
  keyP "new" *> (ENew <$> newPostFixE <*> argsP)

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

    prefixop = Prefix $ foldr1 (.) <$> (some . choice $
      [ unop Neg, unop Pos, unop Not, kunop TypeOf, kunop Void, kunop Await ])

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

-- ---------------------------------------------------------------------------
-- Classes

-- every modifier that can begin a class member
memModTable :: [(MemMod, String)]
memModTable =
  [ (MMStatic,    "static")
  , (MMProtected, "protected")
  , (MMReadonly,  "readonly")
  , (MMAbstract,  "abstract")
  , (MMOverride,  "override")
  ]

-- words that can begin a class member, and so can't be a bare member name.
-- use a string key instead, eg, `"static" = 1`
reservedMemNames :: [String]
reservedMemNames = fmap snd memModTable <> [ "fn", "forall" ]

memModP :: Parser MemMod
memModP = label "modifier" $ choice [ m <$ wordP s | (m, s) <- memModTable ]

memWordP :: Parser String
memWordP = nameWhereP (`notElem` reservedMemNames)

classKeyP :: Parser PropKey
classKeyP = label "member key" $ choice
  [ PKMem . MNPriv <$> privP
  , PKMem . MNPub  <$> memWordP
  , nonShortHandKeyP
  ]

-- class member: modifiers, then a method (`forall`/`fn`) or a field (a key)
classMemP :: Parser ClassMem
classMemP = do
  mods <- many memModP
  methodP mods <|> fieldP mods
  where
    -- fails without consuming unless it sees `forall` or `fn`, so no `try` needed
    methodP mods = do
      vars <- fnHeadP
      CMethod mods <$> classKeyP <*> sigTailP vars <*> optional (fatP *> exprE)
    fieldP mods = CMField mods <$> classKeyP <*> optFlagP <*> annotP <*> initP

classBodyP :: Parser [ClassMem]
classBodyP = keyP "where" *> (snd <$> blockOf classMemP)

classE :: Parser Expr
classE = label "class" $ do
  isAbs          <- (True <$ try (wordP "abstract" *> keyP "class")) <|> (False <$ keyP "class")
  (name, params) <- option (Nothing, []) ((,) . Just <$> identP <*> tparamsP)
  ext            <- optional (keyP "extends" *> trefE)
  impls          <- option [] (keyP "implements" *> sepBy1 trefE commaP)
  EClass . ClassDecl isAbs name params ext impls <$> classBodyP

-- ---------------------------------------------------------------------------
-- Blocks and declarations

blockE :: Parser Expr
blockE = do
  k         <- keyP "where" <|> keyP "do"
  (typ, xs) <- blockOf exprE
  return (EBlock typ (BlockKey k) xs)

declE :: Parser Expr
declE = do
  key <- keyP "let" <|> keyP "const"
  pat <- opE -- exprE would consume the whole declaration (due to assignE)
  ann <- annotP
  symP "="
  EDecl (if key == "let" then LetDecl else ConstDecl) pat ann <$> exprE

typeDeclE :: Parser Expr
typeDeclE = label "type declaration" $
  keyP "type" *> (ETypeDecl <$> identP <*> tparamsP <* symP "=" <*> texprE)

-- parses `opE` and then tries to parse `assignE`
-- if `assignE` fails then the stream is just an operation
opOrAsE :: Parser Expr
opOrAsE = do
  onlyOp <- opE
  assignE onlyOp <|> return onlyOp
  where
    assignE lhs = label "assignment" $ do
      op  <- choice . fmap opP $ [minBound .. maxBound]
      rhs <- exprE
      return (EAssign op lhs rhs)
    opP o = o <$ symP (assignop2str o)

exprE :: Parser Expr
exprE = choice [ lamE, ifthenE, blockE, declE, typeDeclE, classE, opOrAsE ]

-- ---------------------------------------------------------------------------
-- Types

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
  = TOFKeyVal PropKey Bool TExpr   -- Bool: optional `k?: T`
  deriving (Eq, Ord, Show)

data TFuncParam
  = TFPIdent Ident Bool TExpr
  | TFPRest  Ident TExpr
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
  args <- parenP (sepBy texprE commaP) <|> return []
  return $ TEApp name args

ttupE :: Parser TExpr
ttupE = label "type tuple" $
  TETuple <$> squareP (sepBy (TTEPlain <$> texprE) commaP)

tobjE :: Parser TExpr
tobjE = label "type object" $
  TEObj <$> curlyP (sepBy (TOFKeyVal <$> propKeyP <*> optFlagP <* colP <*> texprE) commaP)

-- `fn(...) => R`, after an optional `forall` has been parsed
tfuncE :: [Ident] -> Parser TExpr
tfuncE vars = label "function type" $ do
  keyP "fn"
  ps <- parenP (endByRest paramP restP)
  fatP
  TEFunc vars ps <$> texprE
  where
    paramP = TFPIdent <$> identP <*> optFlagP <* colP <*> texprE
    restP  = TFPRest  <$> (dotsP *> identP) <* colP <*> texprE

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
texprE = choice [ forallP >>= tfuncE, topE ]