{- HLINT ignore "Use <$>" -}
module Expr where

import Control.Monad.Combinators.Expr
import Text.Megaparsec

import Lexer
import Parser
import Data.Function ((&))
import Data.Functor (void)
import Data.Maybe (isJust, isNothing)

data UnOp = Neg | Pos | Not | BNot | TypeOf | Void | Await | Delete
  deriving (Eq, Ord, Show, Enum, Bounded)

unop2str :: UnOp -> String
unop2str o = case o of
  Neg    -> "-"
  Pos    -> "+"
  Not    -> "!"
  BNot   -> "~"
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
  | PKLit  Lit
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
  | OFMethod    PropKey FuncSig Expr
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

data ImportBinds
  = ImpNamed     [ImportSpec]
  | ImpNamespace Ident
  deriving (Eq, Ord, Show)

data ImportClause = ImportClause
  { impDefault :: Maybe Ident
  , impBinds   :: Maybe ImportBinds
  }
  deriving (Eq, Ord, Show)

data ModuleRef = ModuleRef
  { modSource :: String
  , modAttrs  :: [(String, String)]
  }
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

  | ENew        Expr [Elem]
  | ENewTarget

  | EUnOp       UnOp Expr
  | EBinOp      BinOp Expr Expr

  | EAssign     AssignOp Expr Expr

  | EThrow      Expr

  | EImport        ImportClause ModuleRef
  | EImportCall    [Elem]
  | EImportMeta
  | EExportDecl    Expr
  | EExportDefault Expr
  | EExport        ExportClause (Maybe ModuleRef)

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

asP :: Parser ()
asP = wordP "as"

litP :: Parser Lit
litP = matchP $ \case
  TLit l -> Just l
  _      -> Nothing

keyLitP :: Parser Lit
keyLitP = matchP $ \case
  TLit (LStr s) -> Just (LStr s)
  TLit (LNum s) -> Just (LNum s)
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
  TIden s        | p s -> Just s
  TKey  s        | p s -> Just s
  TLit (LNull s) | p s -> Just s
  TLit (LBool s) | p s -> Just s
  _                    -> Nothing

nameP :: Parser String
nameP = nameWhereP (const True)

wordP :: String -> Parser ()
wordP s = label s $
  void $ nameWhereP (== s)

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

propKeyP :: Parser PropKey
propKeyP = label "key" $ choice
  [ PKMem . MNPub <$> nameP
  , PKLit <$> keyLitP
  , PKComp <$> squareP exprE
  ]

classKeyP :: Parser PropKey
classKeyP = label "key" $ (PKMem . MNPriv <$> privP) <|> propKeyP

propKeyStartP :: Parser ()
propKeyStartP = choice
  [ void nameP
  , void keyLitP
  , void (bracketP '[')
  ]

isPropKeyAhead :: Parser ()
isPropKeyAhead = void (lookAhead propKeyStartP)

isMemKeyAhead :: Parser ()
isMemKeyAhead = void (lookAhead (void privP <|> propKeyStartP))

keyColP :: Parser PropKey
keyColP = choice
  [ try (PKMem . MNPub <$> nameP <* colP)
  , PKLit <$> keyLitP <* colP
  , PKComp <$> squareP exprE <* colP
  ]

litE :: Parser Expr
litE = label "literal" $ ELit <$> litP

identE :: Parser Expr
identE = label "identifier" $ EIden <$> identP

privInE :: Parser Expr
privInE = label "private name" $ EPriv . Ident <$> privP <* lookAhead (keyP "in")

importArgsP :: Parser [Elem]
importArgsP = do
  args <- argsP
  case args of
    [ElemPlain _]              -> return args
    [ElemPlain _, ElemPlain _] -> return args
    _                          -> fail "import() takes a module specifier and an optional options object"

importMetaP :: Parser Expr
importMetaP = EImportMeta <$ (symP "." *> wordP "meta")

importExprE :: Parser Expr
importExprE = keyP "import" *> choice [ EImportCall <$> importArgsP, importMetaP ]

importMetaE :: Parser Expr
importMetaE = keyP "import" *> importMetaP

elemE :: Parser Elem
elemE = label "element" $ choice [ ElemSpread <$> (dotsP *> exprE), ElemPlain <$> exprE ]

argsP :: Parser [Elem]
argsP = parenP (commasP elemE)

arrE :: Parser Expr
arrE = label "array" $ EArr <$> squareP (commasP elemE)

objE :: Parser Expr
objE = label "object" $ EObj <$> curlyP (commasP objFieldP)

objFieldP :: Parser ObjField
objFieldP = choice [ spreadP, methodP, keyValP, shortP ]
  where
    spreadP = OFSpread <$> (dotsP *> exprE)
    methodP = do
      isAsync <- try (fnHeadP <* isPropKeyAhead)
      OFMethod <$> propKeyP <*> sigTailP isAsync <*> (fatP *> exprE)
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
  if isNothing mCatch && isNothing mFinally
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
throwE = label "throw" $
  EThrow <$> (keyP "throw" *> exprE)

memE :: Parser (Expr -> Expr)
memE = label "member" $ do
  symP "."
  m <- memNameP
  return $ \x -> EMem x m

appE :: Parser (Expr -> Expr)
appE = label "application" $
  flip EApp <$> argsP

indexE :: Parser (Expr -> Expr)
indexE = label "index" $
  flip EIndex <$> squareP exprE

optPostE :: Parser (Expr -> Expr)
optPostE = label "optional" $ do
  symP "?."
  choice
    [ flip EOptMem   <$> memNameP
    , flip EOptApp   <$> argsP
    , flip EOptIndex <$> squareP exprE
    ]

newE :: Parser Expr
newE = label "new" $ do
  keyP "new"
  newTargetP <|> newCallP
  where
    newTargetP = ENewTarget <$ (symP "." *> wordP "target")
    newCallP = do
      callee <- newCalleeE
      notFollowedBy (symP "?.")
      args <- option [] argsP
      return (ENew callee args)

newCalleeE :: Parser Expr
newCalleeE = do
  prim  <- choice [ newE, identE, litE, arrE, objE, importMetaE, parenP exprE ]
  postL <- many (choice [ memE, indexE ])
  return (foldl (&) prim postL)

incDecP :: Parser a
incDecP = choice
  [ symP "++" *> fail "`++` is not supported; use `+= 1` instead"
  , symP "--" *> fail "`--` is not supported; use `-= 1` instead"
  ]

postfixE :: Parser Expr
postfixE = do
  prim  <- choice [ newE, identE, litE, arrE, objE, privInE, importExprE, parenP exprE ]
  postL <- many (choice [ optPostE, memE, appE, indexE ])
  incDecP <|> return ()
  return (foldl (&) prim postL)

unopP :: Parser UnOp
unopP = label "unary operator" $ choice
  [ Neg    <$ symP "-"
  , Pos    <$ symP "+"
  , Not    <$ symP "!"
  , BNot   <$ symP "~"
  , TypeOf <$ keyP "typeof"
  , Void   <$ keyP "void"
  , Await  <$ keyP "await"
  , Delete <$ keyP "delete"
  ]

unaryE :: Parser Expr
unaryE = choice
  [ EUnOp <$> unopP <*> unaryE
  , incDecP
  , postfixE
  ]

expE :: Parser Expr
expE = choice
  [ EUnOp <$> unopP <*> unaryE
  , incDecP
  , powE
  ]

powE :: Parser Expr
powE = do
  base <- postfixE
  option base (EBinOp Pow base <$> (symP "**" *> expE))

bitE :: Parser Expr
bitE = makeExprParser expE table
  where
    binop o = EBinOp o <$ symP (binop2str o)
    kbin o = EBinOp o <$ keyP (binop2str o)

    table =
      [ [ InfixL (binop Mul), InfixL (binop Div), InfixL (binop Mod) ]
      , [ InfixL (binop Add), InfixL (binop Sub) ]
      , [ InfixL (binop Shl), InfixL (binop Shr), InfixL (binop Ushr) ]
      , [ InfixN (binop Lt), InfixN (binop Lte), InfixN (binop Gt), InfixN (binop Gte)
        , InfixN (kbin InstanceOf), InfixN (kbin In) ]
      , [ InfixN (binop Eq), InfixN (binop Neq) ]
      , [ InfixL (binop BAnd) ]
      , [ InfixL (binop BXor) ]
      , [ InfixL (binop BOr) ]
      ]

andP :: Expr -> Parser Expr
andP x = option x $ do
  symP "&&"
  y <- bitE
  EBinOp And x <$> andP y

orP :: Expr -> Parser Expr
orP x = option x $ do
  symP "||"
  y <- bitE >>= andP
  EBinOp Or x <$> orP y

coalesceP :: Expr -> Parser Expr
coalesceP x = do
  symP "??"
  y <- bitE
  rest <- option y (coalesceP y)
  return (EBinOp Coalesce x rest)

opE :: Parser Expr
opE = do
  first <- bitE
  coalesceP first <|> (andP first >>= orP)

staticP :: Parser Bool
staticP = label "static" $ isJust <$> optional (try (wordP "static" <* isMemKeyAhead))

classMemP :: Parser ClassMem
classMemP = do
  isStatic <- staticP
  methodP isStatic <|> fieldP isStatic
  where
    methodP s = do
      isAsync <- try (fnHeadP <* isMemKeyAhead)
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

attrsP :: Parser [(String, String)]
attrsP = option [] (wordP "with" *> curlyP (commasP attrP))
  where
    attrP = do
      k <- nameP <|> stringP
      colP
      v <- stringP
      return (k, v)

moduleRefP :: Parser ModuleRef
moduleRefP = ModuleRef <$> stringP <*> attrsP

fromP :: Parser ModuleRef
fromP = label "from" $ wordP "from" *> moduleRefP

importSpecP :: Parser ImportSpec
importSpecP = plainP <|> renamedP
  where
    plainP = do
      Ident n <- identP
      ImportSpec n <$> optional (asP *> identP)
    renamedP = do
      n <- nameP <|> stringP
      asP
      ImportSpec n . Just <$> identP

importBindsP :: Parser ImportBinds
importBindsP = namedP <|> namespaceP
  where
    namedP = ImpNamed <$> curlyP (commasP importSpecP)
    namespaceP = do
      symP "*"
      asP
      ImpNamespace <$> identP

importClauseP :: Parser ImportClause
importClauseP = defaultP <|> bindsP
  where
    defaultP = do
      d <- identP
      binds <- optional (commaP *> importBindsP)
      return (ImportClause (Just d) binds)
    bindsP = ImportClause Nothing . Just <$> importBindsP

importE :: Parser Expr
importE = label "import" $ do
  try $ keyP "import" <* notFollowedBy (void (bracketP '(') <|> void (symP "."))
  bareP <|> clauseP
  where
    bareP = EImport (ImportClause Nothing Nothing) <$> moduleRefP
    clauseP = EImport <$> importClauseP <*> fromP

exportNameP :: Parser String
exportNameP = nameP <|> stringP

exportSpecP :: Parser (Bool, ExportSpec)
exportSpecP = do
  isString <- option False (True <$ lookAhead stringP)
  name <- exportNameP
  alias <- optional (asP *> exportNameP)
  return (isString, ExportSpec name alias)

exportE :: Parser Expr
exportE = label "export" $ do
  keyP "export"
  choice
    [ EExportDecl <$> choice [ declE, classE ]
    , defaultP
    , namedP
    , allP
    ]
    where
      defaultP = do
        wordP "default"
        notFollowedBy (keyP "let" <|> keyP "const")
        EExportDefault <$> exprE
      namedP = do
        pairs <- curlyP (commasP exportSpecP)
        mFrom <- optional fromP
        if isNothing mFrom && any fst pairs
          then fail "a string export name needs `from`"
          else return (EExport (ExpNamed (fmap snd pairs)) mFrom)
      allP = do
        symP "*"
        clause <- (ExpAllAs <$> (asP *> exportNameP)) <|> return ExpAll
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
  pat     <- opE
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

programP :: Parser [Expr]
programP = bracketP '<' *> sepEndBy exprE semicolonP <* bracketP '>' <* eofP
