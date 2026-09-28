module Expr where

import Control.Monad.Combinators.Expr
import Text.Megaparsec

import Lexer
import Parser
import Data.Function ((&))
import Data.Functor (void)
import Data.Maybe (isJust)

data UnOp = Neg | Pos | Not | TypeOf | Void | Await | Delete
  deriving (Eq, Ord, Show, Enum, Bounded)

unop2str :: UnOp -> String
unop2str o = case o of
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
binop2str o = case o of
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
assignop2str o = case o of
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

newtype Ident = Ident String
  deriving (Eq, Ord, Show)

data MemName
  = MNPub  String
  | MNPriv String
  deriving (Eq, Ord, Show)

data PropKey
  = PKMem  MemName
  | PKComp Expr
  deriving (Eq, Ord, Show)

data Elem
  = ElemPlain Expr
  | ElemSpread Expr
  deriving (Eq, Ord, Show)

data LamParam
  = LPPlain Expr (Maybe Expr)
  | LPRest  Expr
  deriving (Eq, Ord, Show)

data ObjField
  = OFSpread    Expr
  | OFKeyVal    PropKey Expr
  | OFShort     Ident
  | OFShortInit Ident Expr
  deriving (Eq, Ord, Show)

data ClassMem
  = CMField
  { cmStatic :: Bool
  , cmProp   :: PropKey
  , cmMVal   :: Maybe Expr
  }
  | CMethod
  { cmStatic  :: Bool
  , cmProp    :: PropKey
  , cmFuncSig :: FuncSig
  , cmBody    :: Expr
  }
  deriving (Eq, Ord, Show)

data ClassDecl = ClassDecl
  { cdName    :: Maybe Ident
  , cdExtends :: Maybe Expr
  , cdMembers :: [ClassMem]
  }
  deriving (Eq, Ord, Show)

data ImportSpec = ImportSpec
  { impName  :: String
  , impAlias :: Maybe Ident
  }
  deriving (Eq, Ord, Show)

data ImportClause
  = ImpBare
  | ImpNamed [ImportSpec]
  | ImpNamespace Ident
  deriving (Eq, Ord, Show)

data ExportSpec = ExportSpec
  { expName  :: String
  , expAlias :: Maybe String
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
  , fsParams  :: [LamParam]
  }
  deriving (Eq, Ord, Show)

data TryCatch = TryCatch (Maybe Expr) Expr
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

  | ENew        Expr

  | EUnOp       UnOp Expr
  | EBinOp      BinOp Expr Expr

  | EAssign     AssignOp Expr Expr

  | EThrow      Expr

  | EImport     ImportClause String
  | EImportCall [Elem]
  | EImportMeta
  | EExportDecl Expr
  | EExport     ExportClause (Maybe String)

  | EClass      ClassDecl
  | EDecl       DeclTyp Expr Expr
  | EBlock      BlockTyp BlockKey [Expr]

  | EIfThen     Expr Expr Expr
  | ETry        Expr (Maybe TryCatch) (Maybe Expr)

  | EFor        Bool Expr Expr Expr
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

-- `as` is contextual (only meaningful inside import/export specs)
asP :: Parser ()
asP = wordP "as"

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

initP :: Parser (Maybe Expr)
initP = optional (symP "=" *> exprE)

fnHeadP :: Parser Bool
fnHeadP = do
  isAsync <- (True <$ try (wordP "async" <* lookAhead (keyP "fn"))) <|> return False
  keyP "fn"
  return isAsync

blockOf :: Parser a -> Parser (BlockTyp, [a])
blockOf p = choice
  [ (,) Impl <$> between (bracketP '<') (bracketP '>') items
  , (,) Expl <$> between (bracketP '{') (bracketP '}') items
  ]
  where
    items = sepEndBy p semicolonP

memNameP :: Parser MemName
memNameP = label "member name" $ (MNPub <$> nameP) <|> (MNPriv <$> privP)

keyColP :: Parser PropKey
keyColP = try (PKMem . MNPub <$> nameP <* colP) <|> (PKComp <$> squareP exprE <* colP)

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

sigTailP :: Bool -> Parser FuncSig
sigTailP isAsync = FuncSig isAsync <$> parenP (restListP isRest paramP)
  where
    paramP = plainP <|> restP
    plainP = LPPlain <$> opE <*> initP
    restP  = LPRest  <$> (dotsP *> opE)
    isRest (LPRest _) = True
    isRest _          = False

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

tryE :: Parser Expr
tryE = label "try" $ do
  keyP "try"
  body <- exprE
  mCatch <- optional catchP
  mFinally <- optional (keyP "finally" *> exprE)
  if mCatch == Nothing && mFinally == Nothing
      then fail "try needs a catch or finally clause"
      else return (ETry body mCatch mFinally)
  where
    catchP = do
      keyP "catch"
      binding <- optional opE
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

postfixE :: Parser Expr
postfixE = do
  prim  <- choice [ identE, litE, arrE, objE, privInE, importExprE, parenP exprE ]
  postL <- many $ choice [ optPostE, memE, appE, indexE ]
  return $ foldl (&) prim postL

opE :: Parser Expr
opE = makeExprParser postfixE table
  where
    unop  o = EUnOp o <$ symP (unop2str o)
    kunop o = EUnOp o <$ keyP (unop2str o)
    binop o = EBinOp o <$ symP (binop2str o)
    kbin o = EBinOp o <$ keyP (binop2str o)
    knew = ENew <$ keyP "new"

    prefixop = Prefix $ foldr1 (.) <$> (some . choice $
      [ unop Neg, unop Pos, unop Not, kunop TypeOf, kunop Void, kunop Await, kunop Delete, knew ])

    table =
      [ [ prefixop ]
      , [ InfixR (binop Pow) ]
      , [ InfixL (binop Mul), InfixL (binop Div), InfixL (binop Mod) ]
      , [ InfixL (binop Add), InfixL (binop Sub) ]
      , [ InfixL (binop Shl), InfixL (binop Shr), InfixL (binop Ushr) ]
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

-- `static` is only a modifier when a member key follows; otherwise it's a key
staticP :: Parser Bool
staticP = label "static" $ isJust <$> optional (try (wordP "static" <* isMemKeyAhead))

isMemKeyAhead :: Parser ()
isMemKeyAhead = void . lookAhead . choice . fmap void $
  [ nameP
  , privP
  , (: []) <$> (bracketP '[')
  ]

classKeyP :: Parser PropKey
classKeyP = label "key" $ choice
  [ PKMem . MNPriv <$> privP
  , PKMem . MNPub  <$> nameP
  , PKComp <$> squareP exprE
  ]

classMemP :: Parser ClassMem
classMemP = do
  isStatic <- staticP
  methodP isStatic <|> fieldP isStatic
  where
    methodP s = do
      isAsync <- try (fnHeadP <* lookAhead isMemKeyAhead)
      CMethod s <$> classKeyP <*> sigTailP isAsync <*> (fatP *> exprE)
    fieldP s = CMField s <$> classKeyP <*> initP

classBodyP :: Parser [ClassMem]
classBodyP = keyP "where" *> (snd <$> blockOf classMemP)

classE :: Parser Expr
classE = label "class" $ do
  keyP "class"
  name <- optional identP
  ext  <- optional (keyP "extends" *> postfixE)
  mems <- classBodyP
  return $ EClass $ ClassDecl
    { cdName    = name
    , cdExtends = ext
    , cdMembers = mems
    }

fromP :: Parser String
fromP = label "from" $ wordP "from" *> stringP

importSpecP :: Parser ImportSpec
importSpecP = plainP <|> renamedP
  where
    plainP = do
      Ident n <- identP
      ImportSpec n <$> optional (asP *> identP)
    renamedP = do
      n <- nameP
      asP
      ImportSpec n . Just <$> identP

exportSpecP :: Parser ExportSpec
exportSpecP = ExportSpec <$> nameP <*> optional (asP *> nameP)

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
         asP
         ns <- identP
         EImport (ImpNamespace ns) <$> fromP

exportE :: Parser Expr
exportE = label "export" $ do
  keyP "export"
  choice
    [ EExportDecl <$> choice [ declE, classE ]
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
        clause <- (ExpAllAs <$> (asP *> nameP)) <|> return ExpAll
        EExport clause . Just <$> fromP

blockE :: Parser Expr
blockE = do
  k         <- keyP "where" <|> keyP "do"
  (typ, xs) <- blockOf exprE
  return (EBlock typ (BlockKey k) xs)

declE :: Parser Expr
declE = do
  key <- keyP "let" <|> keyP "const"
  pat <- opE
  symP "="
  EDecl (if key == "let" then LetDecl else ConstDecl) pat <$> exprE

doBodyP :: Parser Expr
doBodyP = do
  keyP "do"
  (typ, xs) <- blockOf exprE
  return (EBlock typ (BlockKey "do") xs)

forE :: Parser Expr
forE = label "for" $ do
  keyP "for"
  isAwait <- isJust <$> optional (keyP "await")
  pat     <- exprE
  wordP "of"
  iter    <- exprE
  EFor isAwait pat iter <$> doBodyP

whileE :: Parser Expr
whileE = label "while" $ keyP "while" *> (EWhile <$> exprE <*> doBodyP)

returnE :: Parser Expr
returnE = label "return" $ keyP "return" *> (EReturn <$> optional exprE)

breakE :: Parser Expr
breakE    = EBreak    <$ keyP "break"

continueE :: Parser Expr
continueE = EContinue <$ keyP "continue"

opOrAsE :: Parser Expr
opOrAsE = do
  onlyOp <- opE
  assignE onlyOp <|> return onlyOp
  where
    assignE lhs = label "assignment" $ do
      o <- choice . fmap opP $ [minBound .. maxBound]
      rhs <- exprE
      return (EAssign o lhs rhs)
    opP o = o <$ symP (assignop2str o)

exprE :: Parser Expr
exprE = choice
  [ lamE, ifthenE, tryE, forE, whileE, blockE, declE, classE
  , throwE, returnE, breakE, continueE, importE, exportE, opOrAsE ]