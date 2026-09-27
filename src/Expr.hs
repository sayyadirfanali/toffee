module Expr where

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Debug

import Utils
import Lexer
import Parser
import Data.Function ((&))
import Data.Functor (void)
import Data.Maybe (isJust, fromMaybe)

data UnOp = Neg | Pos | Not | TypeOf | Void | Await | Delete
  deriving (Eq, Ord, Show, Enum, Bounded)

unop2str :: UnOp -> String
unop2str op = case op of
  Neg    -> "-"
  Pos    -> "+"
  Not    -> "!"
  TypeOf -> "typeof"
  Void   -> "void"
  Await  -> "await"
  Delete -> "delete"

data BinOp
  = Add | Sub | Mul | Div | Mod | Pow
  | Lt | Lte | Gt | Gte | InstanceOf | In
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
  In         -> "in"
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

data CastTyp = CastAs | CastSatisfies
  deriving (Eq, Ord, Show)

newtype Ident = Ident String
  deriving (Eq, Ord, Show)

data MemName
  = MNPub  String
  | MNPriv String
  deriving (Eq, Ord, Show)

data PropKey
  = PKMem  MemName
  | PKStr  String
  | PKNum  String
  | PKComp Expr
  deriving (Eq, Ord, Show)

data Pat
  = PVar Ident
  | PArr [PatElem]
  | PObj [PatField]
  deriving (Eq, Ord, Show)

data PatElem
  = PEHole
  | PEPlain Pat (Maybe Expr)
  | PERest Pat
  deriving (Eq, Ord, Show)

data PatField
  = PFShort Ident (Maybe Expr)
  | PFKey PropKey Pat (Maybe Expr)
  | PFRest Ident
  deriving (Eq, Ord, Show)

data Elem
  = ElemPlain Expr
  | ElemSpread Expr
  deriving (Eq, Ord, Show)

data LamParam
  = LPPlain Pat (Maybe TExpr) (Maybe Expr)
  | LPRest  Pat (Maybe TExpr)
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
  { cdIsAbs   :: Bool
  , cdName    :: Maybe Ident
  , cdParams  :: [Ident]
  , cdExtends :: Maybe TExpr
  , cdImpls   :: [TExpr]
  , cdMembers :: [ClassMem]
  }
  deriving (Eq, Ord, Show)

data ImportSpec = ImportSpec
  { impIsType :: Bool
  , impName   :: String
  , impAlias  :: Maybe Ident
  }
  deriving (Eq, Ord, Show)

data ImportClause
  = ImpBare
  | ImpNamed [ImportSpec]
  | ImpNamespace Ident
  deriving (Eq, Ord, Show)

data ExportSpec = ExportSpec
  { expIsType :: Bool
  , expName   :: String
  , expAlias  :: Maybe String
  }
  deriving (Eq, Ord, Show)

data ExportClause
  = ExpNamed [ExportSpec]
  | ExpAll
  | ExpAllAs String
  deriving (Eq, Ord, Show)

data DeclTyp = LetDecl | ConstDecl
  deriving (Eq, Ord, Show)

data BlockTyp = Expl | Impl
  deriving (Eq, Ord, Show)

newtype BlockKey = BlockKey String
  deriving (Eq, Ord, Show)

data FuncSig = FuncSig
  { fsIsAsync :: Bool
  , fsVars :: [Ident]
  , fsParams  :: [LamParam]
  , fsRet :: Maybe TExpr
  }
  deriving (Eq, Ord, Show)

data TryCatch = TryCatch (Maybe Pat) Expr
  deriving (Eq, Ord, Show)

data InterfaceField = InterfaceField
  { ifIsReadOnly :: Bool
  , ifKey :: PropKey
  , ifIsOpt :: Bool
  , ifType :: TExpr
  }
  deriving (Eq, Ord, Show)

data Expr
  = ELit        Lit
  | EIden       Ident
  | EPriv       Ident

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

  | ECast       CastTyp Expr TExpr
  | EAsConst    Expr

  | EAssign     AssignOp Expr Expr

  | EThrow      Expr

  | EImport     ImportClause String
  | EImportCall [Elem]
  | EImportMeta
  | EExportDecl Expr
  | EExport     ExportClause (Maybe String)

  | EClass      ClassDecl
  | EDecl       DeclTyp Pat (Maybe TExpr) Expr
  | EBlock      BlockTyp BlockKey [Expr]

  | ETypeDecl   Ident [Ident] TExpr

  | EIfThen     Expr Expr Expr
  | ETry        Expr (Maybe TryCatch) (Maybe Expr)
  | EInterface  Ident [Ident] [TExpr] [InterfaceField]

  | EFor        Bool Pat Expr Expr
  | EWhile      Expr Expr
  | EBreak
  | EContinue
  | EReturn     (Maybe Expr)
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
  _             -> Nothing

nameP :: Parser String
nameP = nameWhereP (const True)

wordP :: String -> Parser ()
wordP s = label s (() <$ nameWhereP (== s))

commasP :: Parser a -> Parser [a]
commasP p = sepEndBy p commaP

restLastP :: (a -> Bool) -> [a] -> Parser [a]
restLastP isRest xs
  | any isRest (dropLast xs) = fail "a rest element must come last"
  | otherwise                = return xs
  where
    dropLast ys = zipWith const ys (drop 1 ys)

restListP :: (a -> Bool) -> Parser a -> Parser [a]
restListP isRest p = commasP p >>= restLastP isRest

annotP :: Parser (Maybe TExpr)
annotP = optional (colP *> texprE)

initP :: Parser (Maybe Expr)
initP = optional (symP "=" *> exprE)

optFlagP :: Parser Bool
optFlagP = isJust <$> optional (symP "?")

typeModP :: Parser Bool
typeModP = isJust <$> optional (keyP "type")

tparamsP :: Parser [Ident]
tparamsP = (parenP . commasP $ identP) <|> return []

forallP :: Parser [Ident]
forallP = (keyP "forall" *> some identP <* symP ".") <|> return []

fnHeadP :: Parser (Bool, [Ident])
fnHeadP = do
  isAsync <- (True <$ try (wordP "async" <* lookAhead (keyP "fn" <|> keyP "forall"))) <|> return False
  vars    <- forallP
  keyP "fn"
  return (isAsync, vars)

blockOf :: Parser a -> Parser (BlockTyp, [a])
blockOf p = choice
  [ (,) Impl <$> between (bracketP '<') (bracketP '>') items
  , (,) Expl <$> between (bracketP '{') (bracketP '}') items
  ]
  where
    items = sepEndBy p semicolonP

memNameP :: Parser MemName
memNameP = label "member name" $ (MNPub <$> nameP) <|> (MNPriv <$> privP)

nonShortHandKeyP :: Parser PropKey
nonShortHandKeyP = choice
  [ PKStr  <$> stringP
  , PKNum  <$> numP
  , PKComp <$> squareP exprE
  ]

propKeyP :: Parser PropKey
propKeyP = label "property key" $ (PKMem . MNPub <$> nameP) <|> nonShortHandKeyP

keyColP :: Parser PropKey
keyColP = try (PKMem . MNPub <$> nameP <* colP) <|> (nonShortHandKeyP <* colP)

contKeyP :: String -> Parser String
contKeyP k = try (optional semicolonP *> keyP k)

patP :: Parser Pat
patP = label "pattern" $ choice
  [ PVar <$> identP
  , PArr <$> arrPatP
  , PObj <$> objPatP
  ]

arrPatP :: Parser [PatElem]
arrPatP = squareP $ do
  slots <- sepBy (optional elemP) commaP
  restLastP isRest (fromMaybe PEHole <$> dropTrailingEmpty slots)
  where
    elemP = (PERest <$> (dotsP *> patP)) <|> (PEPlain <$> patP <*> initP)
    dropTrailingEmpty xs = case reverse xs of
      Nothing : ys -> reverse ys
      _            -> xs
    isRest (PERest _) = True
    isRest _          = False

objPatP :: Parser [PatField]
objPatP = curlyP (restListP isRest fieldP)
  where
    fieldP  = choice [ restP, keyedP, shortP ]
    restP   = PFRest  <$> (dotsP *> identP)
    keyedP  = PFKey   <$> keyColP <*> patP <*> initP
    shortP  = PFShort <$> identP <*> initP
    isRest (PFRest _) = True
    isRest _          = False

litE :: Parser Expr
litE = label "literal" $ ELit <$> litP

identE :: Parser Expr
identE = label "identifier" $ EIden <$> identP

privInE :: Parser Expr
privInE = label "private name" $ (EPriv . Ident) <$> privP <* lookAhead (keyP "in")

importExprE :: Parser Expr
importExprE = keyP "import" *> choice
  [ EImportCall <$> argsP
  , EImportMeta <$ (symP "." *> wordP "meta")
  ]

elemE :: Parser Elem
elemE = label "element" $ choice [ ElemSpread <$> (dotsP *> exprE), ElemPlain <$> exprE ]

argsP :: Parser [Elem]
argsP = parenP (commasP elemE)

arrE :: Parser Expr
arrE = label "array" $ EArr <$> squareP (commasP elemE)

objE :: Parser Expr
objE = label "object" $ EObj <$> curlyP (commasP fieldP)
  where
    fieldP  = choice [ spreadP, keyValP, shortP ]
    spreadP = OFSpread <$> (dotsP *> exprE)
    keyValP = OFKeyVal <$> keyColP <*> exprE
    shortP = do
      i <- identP
      (OFShortInit i <$> (symP "=" *> exprE)) <|> return (OFShort i)

sigTailP :: (Bool, [Ident]) -> Parser FuncSig
sigTailP (isAsync, vars) = FuncSig isAsync vars <$> parenP (restListP isRest paramP) <*> annotP
  where
    paramP = plainP <|> restP
    plainP = LPPlain <$> patP <*> annotP <*> initP
    restP  = LPRest  <$> (dotsP *> patP) <*> annotP
    isRest (LPRest _ _) = True
    isRest _            = False

funcsigP :: Parser FuncSig
funcsigP = fnHeadP >>= sigTailP

lamE :: Parser Expr
lamE = label "lambda" $ ELam <$> funcsigP <* fatP <*> exprE

ifthenE :: Parser Expr
ifthenE = label "if then" $ do
  keyP "if"
  cond <- exprE
  contKeyP "then"
  expThen <- exprE
  contKeyP "else"
  expElse <- exprE
  return $ EIfThen cond expThen expElse

tryE :: Parser Expr
tryE = label "try" $ do
  keyP "try"
  body <- exprE
  mCatch <- optional catchP
  mFinally <- optional (contKeyP "finally" *> exprE)
  if mCatch == Nothing && mFinally == Nothing
      then fail "try needs a catch or finally clause"
      else return (ETry body mCatch mFinally)
  where
    catchP = do
      contKeyP "catch"
      binding <- optional patP
      fatP
      handler <- exprE
      return $ TryCatch binding handler

throwE :: Parser Expr
throwE = label "throw" $ EThrow <$> (keyP "throw" *> exprE)

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
  prim  <- choice [ identE, litE, arrE, objE, newE, privInE, importExprE, parenP exprE ]
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
      [ unop Neg, unop Pos, unop Not, kunop TypeOf, kunop Void, kunop Await, kunop Delete ])

    castop = Postfix $ foldl1 (flip (.)) <$> some (choice [ asP, satP ])
    asP    = keyP "as" *> choice
      [ EAsConst <$ keyP "const"
      , flip (ECast CastAs) <$> texprE
      ]
    satP   = keyP "satisfies" *> (flip (ECast CastSatisfies) <$> texprE)

    table =
      [ [ prefixop ]
      , [ InfixR (binop Pow) ]
      , [ InfixL (binop Mul), InfixL (binop Div), InfixL (binop Mod) ]
      , [ InfixL (binop Add), InfixL (binop Sub) ]
      , [ InfixL (binop Shl), InfixL (binop Shr), InfixL (binop Ushr) ]
      , [ castop ]
      , [ InfixN (binop Lt), InfixN (binop Lte), InfixN (binop Gt), InfixN (binop Gte)
        , InfixN (kbin InstanceOf), InfixN (kbin In) ]
      , [ InfixN (binop Eq), InfixN (binop Neq) ]
      , [ InfixL (binop BAnd) ]
      , [ InfixL (binop BXor) ]
      , [ InfixL (binop BOr) ]
      , [ InfixR (binop And) ]
      , [ InfixR (binop Or) ]
      , [ InfixR (binop Coalesce) ]
      ]

memModTable :: [(MemMod, String)]
memModTable =
  [ (MMStatic, "static")
  , (MMProtected, "protected")
  , (MMReadonly, "readonly")
  , (MMAbstract, "abstract")
  , (MMOverride, "override")
  ]

notMemNames :: [String]
notMemNames = fmap snd memModTable <> [ "fn", "forall", "async" ]

memModP :: Parser MemMod
memModP = label "modifier" $ choice [ m <$ wordP s | (m, s) <- memModTable ]

memWordP :: Parser String
memWordP = nameWhereP (`notElem` notMemNames)

classKeyP :: Parser PropKey
classKeyP = label "member key" $ choice
  [ PKMem . MNPriv <$> privP
  , PKMem . MNPub  <$> memWordP
  , nonShortHandKeyP
  ]

classMemP :: Parser ClassMem
classMemP = do
  mods <- many memModP
  methodP mods <|> fieldP mods
  where
    methodP mods = do
      hd <- fnHeadP
      CMethod mods <$> classKeyP <*> sigTailP hd <*> optional (fatP *> exprE)
    fieldP mods = CMField mods <$> classKeyP <*> optFlagP <*> annotP <*> initP

classBodyP :: Parser [ClassMem]
classBodyP = keyP "where" *> (snd <$> blockOf classMemP)

classE :: Parser Expr
classE = label "class" $ do
  isAbs <- (True <$ try (wordP "abstract" *> keyP "class")) <|> (False <$ keyP "class")
  (name, params) <- ((,) . Just <$> identP <*> tparamsP) <|> return (Nothing, [])
  ext   <- optional (keyP "extends" *> trefE)
  impls <- (keyP "implements" *> sepBy1 trefE commaP) <|> return []
  mems  <- classBodyP
  return $ EClass $ ClassDecl
    { cdIsAbs   = isAbs
    , cdName    = name
    , cdParams  = params
    , cdExtends = ext
    , cdImpls   = impls
    , cdMembers = mems
    }

interfaceE :: Parser Expr
interfaceE = label "interface" $ do
  keyP "interface"
  name <- identP
  params <- tparamsP
  exts <- (keyP "extends" *> sepBy1 trefE commaP) <|> return []
  keyP "where"
  fields <- snd <$> blockOf fieldP
  return $ EInterface name params exts fields
  where
    fieldP = InterfaceField
      <$> (isJust <$> optional (wordP "readonly"))
      <*> keyP'
      <*> optFlagP
      <*  colP
      <*> texprE
    keyP' = label "field key" $
      (PKMem . MNPub <$> nameWhereP (/= "readonly")) <|> nonShortHandKeyP


fromP :: Parser String
fromP = label "from" $ wordP "from" *> stringP

importSpecP :: Parser ImportSpec
importSpecP = do
  isType <- typeModP
  plainP isType <|> renamedP isType
  where
    plainP t = do
      Ident n <- identP
      ImportSpec t n <$> optional (keyP "as" *> identP)
    renamedP t = do
      n <- nameP
      keyP "as"
      ImportSpec t n . Just <$> identP

exportSpecP :: Parser ExportSpec
exportSpecP = ExportSpec <$> typeModP <*> nameP <*> optional (keyP "as" *> nameP)

importE :: Parser Expr
importE = label "import" $ do
  try $ keyP "import" <* notFollowedBy (void (bracketP '(') <|> void (symP "."))
  choice [ bareP, namedP, namespaceP ]
    where
      bareP = EImport ImpBare <$> stringP
      namedP = do
        specs <- curlyP (commasP importSpecP)
        EImport (ImpNamed specs) <$> fromP
      namespaceP = do
         symP "*"
         keyP "as"
         ns <- identP
         EImport (ImpNamespace ns) <$> fromP

exportE :: Parser Expr
exportE = label "export" $ do
  keyP "export"
  choice
    [ EExportDecl <$> choice [ declE, typeDeclE, classE, interfaceE ]
    , namedP
    , allP
    ]
    where
      namedP = do
        specs <- curlyP (commasP exportSpecP)
        mFrom <- optional fromP
        return $ EExport (ExpNamed specs) mFrom
      allP = do
        symP "*"
        clause <- (ExpAllAs <$> (keyP "as" *> nameP)) <|> return ExpAll
        EExport clause . Just <$> fromP

blockE :: Parser Expr
blockE = do
  k         <- keyP "where" <|> keyP "do"
  (typ, xs) <- blockOf exprE
  return (EBlock typ (BlockKey k) xs)

declE :: Parser Expr
declE = do
  key <- keyP "let" <|> keyP "const"
  pat <- patP
  ann <- annotP
  symP "="
  EDecl (if key == "let" then LetDecl else ConstDecl) pat ann <$> exprE

doBodyP :: Parser Expr
doBodyP = do
  keyP "do"
  (typ, xs) <- blockOf exprE
  return (EBlock typ (BlockKey "do") xs)

forE :: Parser Expr
forE = label "for" $ do
  keyP "for"
  isAwait <- isJust <$> optional (keyP "await")
  pat     <- patP
  wordP "of"
  iter    <- exprE
  EFor isAwait pat iter <$> doBodyP

whileE :: Parser Expr
whileE = label "while" $ keyP "while" *> (EWhile <$> exprE <*> doBodyP)

returnE :: Parser Expr
returnE = label "return" $ keyP "return" *> (EReturn <$> optional exprE)

breakE, continueE :: Parser Expr
breakE    = EBreak    <$ keyP "break"
continueE = EContinue <$ keyP "continue"
typeDeclE :: Parser Expr
typeDeclE = label "type declaration" $
  keyP "type" *> (ETypeDecl <$> identP <*> tparamsP <* symP "=" <*> texprE)

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
exprE = choice
  [ lamE, ifthenE, tryE, forE, whileE, blockE, declE, typeDeclE, classE, interfaceE , throwE, returnE, breakE, continueE, importE, exportE, opOrAsE ]

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
  = TOFKeyVal PropKey Bool TExpr
  deriving (Eq, Ord, Show)

data TFuncParam
  = TFPIdent { tfpiName :: Ident, tfpIsOpt :: Bool, tfpType :: TExpr }
  | TFPRest  { tfprName :: Ident, tfprType :: TExpr }
  deriving (Eq, Ord, Show)

newtype Raw = Raw String
  deriving (Eq, Ord, Show)

data TExpr
  = TELit Lit
  | TEApp [Ident] [TExpr]
  | TEArr TExpr
  | TETuple [TTupElem]
  | TEUnOp TUnOp TExpr
  | TEBinOp TBinOp TExpr TExpr
  | TEObj [TObjField]
  | TEFunc { tefForall :: [Ident], tefParams :: [TFuncParam], tefRet :: TExpr }
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
  args <- parenP (commasP texprE) <|> return []
  return $ TEApp name args

ttupE :: Parser TExpr
ttupE = label "type tuple" $ TETuple <$> squareP (commasP (TTEPlain <$> texprE))

tobjE :: Parser TExpr
tobjE = label "type object" $
  TEObj <$> curlyP (commasP (TOFKeyVal <$> propKeyP <*> optFlagP <* colP <*> texprE))

tfuncE :: Parser TExpr
tfuncE = label "function type" $ do
  vars <- forallP
  keyP "fn"
  params <- parenP (restListP isRest paramP)
  fatP
  ret <- texprE
  return $ TEFunc { tefForall = vars, tefParams = params, tefRet = ret }
  where
    paramP = plainP <|> restP
    plainP = TFPIdent <$> identP <*> optFlagP <* colP <*> texprE
    restP  = TFPRest  <$> (dotsP *> identP) <* colP <*> texprE
    isRest (TFPRest _ _) = True
    isRest _             = False

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
texprE = choice [ tfuncE, topE ]