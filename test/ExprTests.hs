module ExprTests (tests) where

import Test.HUnit
import qualified Data.List.NonEmpty as NE
import Text.Megaparsec (errorBundlePretty, bundleErrors, parseErrorTextPretty)
import Text.Megaparsec (sepEndBy)

import Expr
import Layout (layout)
import Lexer
import Parser

topP :: Parser [Expr]
topP = bracketP '<' *> sepEndBy exprE semicolonP <* bracketP '>' <* eofP

runTop :: String -> IO (Either String [Expr])
runTop inp = case runLex inp of
  Left e   -> return $ Left ("lex error: " <> errorBundlePretty e)
  Right ts -> case layout ts of
    Left e   -> return $ Left ("layout error: " <> show e)
    Right ys -> do
      r <- runParse topP ys
      return $ case r of
        Left e  -> Left $ concatMap parseErrorTextPretty (NE.toList (bundleErrors e))
        Right x -> Right x

astTop :: String -> [Expr] -> Test
astTop inp exp = TestCase $ do
  res <- runTop inp
  case res of
    Left err  -> assertFailure ("failed on " <> show inp <> ": " <> err)
    Right out -> out @?= exp

failTop :: String -> Test
failTop inp = TestCase $ do
  res <- runTop inp
  case res of
    Left _    -> return ()
    Right out -> assertFailure ("expected failure on " <> show inp <> ", got " <> show out)

run :: Parser a -> String -> IO (Either String a)
run p s = case runLex s of
  Left e   -> return (Left ("lex error: " <> errorBundlePretty e))
  Right ts -> do
    r <- runParse (p <* eofP) ts
    return $ case r of
      Left b  -> Left (concatMap parseErrorTextPretty (NE.toList (bundleErrors b)))
      Right x -> Right x

parenTest :: String -> String -> Test
parenTest inp exp = TestCase $ do
  res <- run exprE inp
  case res of
    Left err   -> assertFailure ("parse failed on " <> show inp <> ": " <> err)
    Right out  -> paren out @?= exp
  where
    paren :: Expr -> String
    paren (ELit l) = litDat l
    paren (EIden (Ident s)) = s
    paren (EUnOp op x) = "(" <> unop2str op <> " " <> paren x <> ")"
    paren (EBinOp op a b) = "(" <> paren a <> " " <> binop2str op <> " " <> paren b <> ")"
    paren e = show e

astTest :: String -> Expr -> Test
astTest inp exp = TestCase $ do
  res <- run exprE inp
  case res of
    Left err  -> assertFailure ("parse failed on " <> show inp <> ": " <> err)
    Right out -> out @?= exp

failTest :: String -> Test
failTest inp = TestCase $ do
  res <- run exprE inp
  case res of
    Left _    -> return ()
    Right out -> assertFailure ("expected parse failure on " <> show inp <> ", got " <> show out)


iden :: String -> Expr
iden = EIden . Ident

priv :: String -> Expr
priv = EPriv . Ident

num :: String -> Expr
num = ELit . LNum

str :: String -> Expr
str = ELit . LStr

tref :: String -> TExpr
tref n = TEApp [Ident n] []

args :: [Expr] -> [Elem]
args = fmap ElemPlain

pv :: String -> Pat
pv = PVar . Ident

pel :: Pat -> PatElem
pel p = PEPlain p Nothing

pfs :: String -> PatField
pfs x = PFShort (Ident x) Nothing

pfk :: String -> Pat -> PatField
pfk k p = PFKey (key k) p Nothing

param :: String -> LamParam
param x = LPPlain (pv x) Nothing Nothing

paramT :: String -> TExpr -> LamParam
paramT x t = LPPlain (pv x) (Just t) Nothing

paramD :: String -> Expr -> LamParam
paramD x d = LPPlain (pv x) Nothing (Just d)

paramP :: Pat -> LamParam
paramP p = LPPlain p Nothing Nothing

rest :: String -> LamParam
rest x = LPRest (pv x) Nothing

lam :: [LamParam] -> Expr -> Expr
lam ps = ELam (FuncSig False [] ps Nothing)

alam :: [LamParam] -> Expr -> Expr
alam ps = ELam (FuncSig True [] ps Nothing)

letD, constD :: Pat -> Expr -> Expr
letD   p = EDecl LetDecl   p Nothing
constD p = EDecl ConstDecl p Nothing

key :: String -> PropKey
key = PKMem . MNPub

sig0 :: FuncSig
sig0 = FuncSig False [] [] Nothing

cdecl :: Bool -> Maybe String -> [String] -> Maybe TExpr -> [TExpr] -> [ClassMem] -> Expr
cdecl a n ps e is ms = EClass (ClassDecl a (Ident <$> n) (Ident <$> ps) e is ms)

cls :: String -> [ClassMem] -> Expr
cls n = cdecl False (Just n) [] Nothing []

fieldI :: String -> Expr -> ClassMem
fieldI k e = CMField [] (key k) False Nothing (Just e)

meth :: String -> [LamParam] -> Expr -> ClassMem
meth k ps body = CMethod [] (key k) (FuncSig False [] ps Nothing) (Just body)

castAs, castSat :: Expr -> TExpr -> Expr
castAs  = ECast CastAs
castSat = ECast CastSatisfies

imp :: String -> ImportSpec
imp n = ImportSpec False n Nothing

impAs :: String -> String -> ImportSpec
impAs n a = ImportSpec False n (Just (Ident a))

expo :: String -> ExportSpec
expo n = ExportSpec False n Nothing

expoAs :: String -> String -> ExportSpec
expoAs n a = ExportSpec False n (Just a)

catch :: Maybe String -> Expr -> Maybe TryCatch
catch v h = Just (TryCatch (pv <$> v) h)

dob :: [Expr] -> Expr
dob = EBlock Expl (BlockKey "do")

ifld :: String -> TExpr -> InterfaceField
ifld k t = InterfaceField False (key k) False t

tests :: Test
tests = TestList $ concat
  [ atoms
  , arrays
  , objects
  , postfix
  , privateMembers
  , spreadArgs
  , newTests
  , unary
  , binary
  , precedence
  , nonAssoc
  , casts
  , ins
  , deletes
  , lambdas
  , params
  , signatures
  , asyncs
  , trailingCommas
  , conditionals
  , tries
  , contKeywords
  , throws
  , assignment
  , declarations
  , annotatedDecls
  , patterns
  , keywordBindings
  , typeDecls
  , classes
  , reservedMembers
  , imports
  , exports
  , blocks
  , loops
  , interfaces
  , grouping
  , topLevel
  , topFailures
  ]

atoms :: [Test]
atoms =
  [ astTest "x" (iden "x")
  , astTest "_x" (iden "_x")
  , astTest "42" (num "42")
  , astTest "1.5" (num "1.5")
  , astTest "\"a\"" (str "a")
  , astTest "true" (ELit (LBool "true"))
  , astTest "null" (ELit (LNull "null"))
  , astTest "false" (ELit (LBool "false"))
  ]

arrays :: [Test]
arrays =
  [ astTest "[]" (EArr [])
  , astTest "[1]" (EArr [ElemPlain (num "1")])
  , astTest "[1, 2]" (EArr [ElemPlain (num "1"), ElemPlain (num "2")])
  , astTest "[1, ...xs, 2]" (EArr [ElemPlain (num "1"), ElemSpread (iden "xs"), ElemPlain (num "2")])
  , astTest "[...xs]" (EArr [ElemSpread (iden "xs")])
  , astTest "[[1], [2]]" (EArr [ElemPlain (EArr [ElemPlain (num "1")]), ElemPlain (EArr [ElemPlain (num "2")])])
  , astTest "[a + b]" (EArr [ElemPlain (EBinOp Add (iden "a") (iden "b"))])
  , astTest "[fn(x) => x]" (EArr [ElemPlain (lam [param "x"] (iden "x"))])
  , astTest "[a = 1]" (EArr [ElemPlain (EAssign AEq (iden "a") (num "1"))])

  , failTest "[1, 2"
  , failTest "[,]"
  , failTest "[...]"
  ]

objects :: [Test]
objects =
  [ astTest "{}" (EObj [])
  , astTest "{ a: 1 }" (EObj [OFKeyVal (key "a") (num "1")])
  , astTest "{ a }" (EObj [OFShort (Ident "a")])
  , astTest "{ ...o }" (EObj [OFSpread (iden "o")])
  , astTest "{ [k]: v }" (EObj [OFKeyVal (PKComp (iden "k")) (iden "v")])
  , astTest "{ [a + b]: 1 }" (EObj [OFKeyVal (PKComp (EBinOp Add (iden "a") (iden "b"))) (num "1")])
  , astTest "{ \"a\": 1 }" (EObj [OFKeyVal (PKStr "a") (num "1")])
  , astTest "{ \"a-b\": 1 }" (EObj [OFKeyVal (PKStr "a-b") (num "1")])
  , astTest "{ 1: x }" (EObj [OFKeyVal (PKNum "1") (iden "x")])
  , astTest "{ 1.5: x }" (EObj [OFKeyVal (PKNum "1.5") (iden "x")])
  , astTest "{ if: 1 }" (EObj [OFKeyVal (key "if") (num "1")])
  , astTest "{ new: 1, if: 2 }" (EObj [OFKeyVal (key "new") (num "1"), OFKeyVal (key "if") (num "2")])
  , astTest "{ a: { b: 1 } }" (EObj [OFKeyVal (key "a") (EObj [OFKeyVal (key "b") (num "1")])])
  , astTest "{ a: 1, b, ...c, [d]: e, \"f\": g, 0: h }"
      (EObj
        [ OFKeyVal (key "a") (num "1")
        , OFShort  (Ident "b")
        , OFSpread (iden "c")
        , OFKeyVal (PKComp (iden "d")) (iden "e")
        , OFKeyVal (PKStr "f") (iden "g")
        , OFKeyVal (PKNum "0") (iden "h")
        ])

  , astTest "{ a = 1 }" (EObj [OFShortInit (Ident "a") (num "1")])
  , astTest "{ a: b = 1 }" (EObj [OFKeyVal (key "a") (EAssign AEq (iden "b") (num "1"))])

  , failTest "{ if }"
  , failTest "{ a: }"
  , failTest "{ a 1 }"
  , failTest "{ a: 1"

  , failTest "{ \"a\" }"
  , failTest "{ 1 }"
  , failTest "{ [k] }"
  , failTest "{ \"a\" = 1 }"
  , failTest "{ [k]: }"

  , failTest "{ #a: 1 }"
  , failTest "{ #a }"
  ]

postfix :: [Test]
postfix =
  [ astTest "a.b" (EMem (iden "a") (MNPub "b"))
  , astTest "a . b" (EMem (iden "a") (MNPub "b"))
  , astTest "a.b.c" (EMem (EMem (iden "a") (MNPub "b")) (MNPub "c"))
  , astTest "a.if" (EMem (iden "a") (MNPub "if"))
  , astTest "a[0]" (EIndex (iden "a") (num "0"))
  , astTest "a[b + 1]" (EIndex (iden "a") (EBinOp Add (iden "b") (num "1")))
  , astTest "f()" (EApp (iden "f") [])
  , astTest "f(a)" (EApp (iden "f") (args [iden "a"]))
  , astTest "f(a, b)" (EApp (iden "f") (args [iden "a", iden "b"]))
  , astTest "f()(b)" (EApp (EApp (iden "f") []) (args [iden "b"]))
  , astTest "a.b(c).d[0]"
      (EIndex
         (EMem
            (EApp (EMem (iden "a") (MNPub "b")) (args [iden "c"]))
            (MNPub "d"))
         (num "0"))

  , astTest "a?.b" (EOptMem (iden "a") (MNPub "b"))
  , astTest "a?.(x)" (EOptApp (iden "a") (args [iden "x"]))
  , astTest "a?.[0]" (EOptIndex (iden "a") (num "0"))
  , astTest "a?.b.c" (EMem (EOptMem (iden "a") (MNPub "b")) (MNPub "c"))
  , astTest "a?.b?.c" (EOptMem (EOptMem (iden "a") (MNPub "b")) (MNPub "c"))

  , astTest "x!" (ENonNull (iden "x"))
  , astTest "x!.y" (EMem (ENonNull (iden "x")) (MNPub "y"))
  , astTest "x![0]" (EIndex (ENonNull (iden "x")) (num "0"))
  , astTest "f(x)!" (ENonNull (EApp (iden "f") (args [iden "x"])))
  , astTest "a.b!.c" (EMem (ENonNull (EMem (iden "a") (MNPub "b"))) (MNPub "c"))

  , failTest "a."
  , failTest "a?."
  , failTest "a.1"
  , failTest "a[]"
  , failTest "f(a"
  ]

privateMembers :: [Test]
privateMembers =
  [ astTest "this.#x" (EMem (iden "this") (MNPriv "x"))
  , astTest "a?.#x" (EOptMem (iden "a") (MNPriv "x"))
  , astTest "this.#x.y" (EMem (EMem (iden "this") (MNPriv "x")) (MNPub "y"))
  , astTest "this.#f()" (EApp (EMem (iden "this") (MNPriv "f")) [])
  , astTest "this.#xs[0]" (EIndex (EMem (iden "this") (MNPriv "xs")) (num "0"))
  , astTest "this.#x!" (ENonNull (EMem (iden "this") (MNPriv "x")))
  , astTest "a.#if" (EMem (iden "a") (MNPriv "if"))
  , astTest "this.#x = 1" (EAssign AEq (EMem (iden "this") (MNPriv "x")) (num "1"))
  , astTest "this.#n += 1" (EAssign AAdd (EMem (iden "this") (MNPriv "n")) (num "1"))
  , astTest "{ a: this.#x }" (EObj [OFKeyVal (key "a") (EMem (iden "this") (MNPriv "x"))])

  , astTest "a?.new" (EOptMem (iden "a") (MNPub "new"))

  , failTest "#x"
  , failTest "a.#"
  , failTest "a.#1"
  , failTest "let #x = 1"
  , failTest "fn(#x) => x"
  ]

spreadArgs :: [Test]
spreadArgs =
  [ astTest "f(...xs)" (EApp (iden "f") [ElemSpread (iden "xs")])
  , astTest "f(a, ...xs)" (EApp (iden "f") [ElemPlain (iden "a"), ElemSpread (iden "xs")])
  , astTest "f(a, ...xs, b)" (EApp (iden "f") [ElemPlain (iden "a"), ElemSpread (iden "xs"), ElemPlain (iden "b")])
  , astTest "f(...a, ...b)" (EApp (iden "f") [ElemSpread (iden "a"), ElemSpread (iden "b")])
  , astTest "f(...a + b)" (EApp (iden "f") [ElemSpread (EBinOp Add (iden "a") (iden "b"))])
  , astTest "f(...xs.ys)" (EApp (iden "f") [ElemSpread (EMem (iden "xs") (MNPub "ys"))])
  , astTest "f(...xs)(y)" (EApp (EApp (iden "f") [ElemSpread (iden "xs")]) (args [iden "y"]))
  , astTest "a?.(...xs)" (EOptApp (iden "a") [ElemSpread (iden "xs")])
  , astTest "new Foo(...args)" (ENew (iden "Foo") [ElemSpread (iden "args")])
  , astTest "new Foo(a, ...bs)" (ENew (iden "Foo") [ElemPlain (iden "a"), ElemSpread (iden "bs")])

  , failTest "f(...)"
  , failTest "f(... , a)"
  , failTest "f(xs...)"
  ]

newTests :: [Test]
newTests =
  [ astTest "new Foo()" (ENew (iden "Foo") [])
  , astTest "new Foo(x)" (ENew (iden "Foo") (args [iden "x"]))
  , astTest "new Foo(x, y)" (ENew (iden "Foo") (args [iden "x", iden "y"]))
  , astTest "new a.b.C(x)" (ENew (EMem (EMem (iden "a") (MNPub "b")) (MNPub "C")) (args [iden "x"]))
  , astTest "new Foo[0](x)" (ENew (EIndex (iden "Foo") (num "0")) (args [iden "x"]))
  , astTest "new (f())(x)" (ENew (EApp (iden "f") []) (args [iden "x"]))

  , astTest "new Foo().bar" (EMem (ENew (iden "Foo") []) (MNPub "bar"))
  , astTest "new Foo()(x)" (EApp (ENew (iden "Foo") []) (args [iden "x"]))

  , failTest "new Foo"
  , failTest "new"
  , failTest "new a?.b()"
  ]

unary :: [Test]
unary =
  [ astTest (unop2str op <> " x") (EUnOp op (iden "x")) | op <- [minBound .. maxBound] ]
  <>
  [ astTest "-x" (EUnOp Neg (iden "x"))
  , astTest "!x" (EUnOp Not (iden "x"))
  , astTest "- -x" (EUnOp Neg (EUnOp Neg (iden "x")))
  , astTest "--x" (EUnOp Neg (EUnOp Neg (iden "x")))
  , astTest "!!x" (EUnOp Not (EUnOp Not (iden "x")))
  , astTest "typeof typeof x" (EUnOp TypeOf (EUnOp TypeOf (iden "x")))
  , astTest "void 0" (EUnOp Void (num "0"))
  , astTest "await f()" (EUnOp Await (EApp (iden "f") []))

  , astTest "-x.y" (EUnOp Neg (EMem (iden "x") (MNPub "y")))
  , astTest "-f(x)" (EUnOp Neg (EApp (iden "f") (args [iden "x"])))
  , astTest "!x!" (EUnOp Not (ENonNull (iden "x")))

  , failTest "-"
  , failTest "typeof"
  ]

binary :: [Test]
binary =
  [ astTest ("a " <> binop2str op <> " b") (EBinOp op (iden "a") (iden "b")) | op <- [minBound ..] ]

precedence :: [Test]
precedence =
  [ parenTest "a + b * c" "(a + (b * c))"
  , parenTest "a * b + c" "((a * b) + c)"
  , parenTest "a - b - c" "((a - b) - c)"
  , parenTest "a - b + c" "((a - b) + c)"
  , parenTest "a / b / c" "((a / b) / c)"
  , parenTest "a / b * c" "((a / b) * c)"
  , parenTest "a * b % c" "((a * b) % c)"
  , parenTest "a % b + c" "((a % b) + c)"
  , parenTest "a ** b ** c" "(a ** (b ** c))"
  , parenTest "a * b ** c" "(a * (b ** c))"
  , parenTest "a + b << c" "((a + b) << c)"
  , parenTest "a << b + c" "(a << (b + c))"
  , parenTest "a >> b >>> c" "((a >> b) >>> c)"
  , parenTest "a << b < c" "((a << b) < c)"
  , parenTest "a < b == c" "((a < b) == c)"
  , parenTest "a + b instanceof c" "((a + b) instanceof c)"
  , parenTest "a instanceof b == c" "((a instanceof b) == c)"
  , parenTest "a + b in c" "((a + b) in c)"
  , parenTest "a in b == c" "((a in b) == c)"
  , parenTest "a == b & c" "((a == b) & c)"
  , parenTest "a & b == c" "(a & (b == c))"
  , parenTest "a & b ^ c" "((a & b) ^ c)"
  , parenTest "a ^ b | c" "((a ^ b) | c)"
  , parenTest "a | b & c" "(a | (b & c))"
  , parenTest "a | b && c" "((a | b) && c)"
  , parenTest "a && b || c" "((a && b) || c)"
  , parenTest "a && b && c" "(a && (b && c))"
  , parenTest "a || b || c" "(a || (b || c))"
  , parenTest "a || b ?? c" "((a || b) ?? c)"
  , parenTest "a ?? b ?? c" "(a ?? (b ?? c))"
  , parenTest "a < b == c < d" "((a < b) == (c < d))"
  , parenTest "a != b" "(a != b)"

  , parenTest "-a * b" "((- a) * b)"
  , parenTest "-a ** b" "((- a) ** b)"
  , parenTest "-a + b" "((- a) + b)"
  , parenTest "a + -b" "(a + (- b))"
  , parenTest "a ** -b" "(a ** (- b))"
  , parenTest "- -a" "(- (- a))"
  , parenTest "!a && b" "((! a) && b)"
  , parenTest "!a == b" "((! a) == b)"
  , parenTest "typeof -x" "(typeof (- x))"
  , parenTest "typeof a + b" "((typeof a) + b)"
  , parenTest "delete a && b" "((delete a) && b)"

  , parenTest "(a + b) * c" "((a + b) * c)"
  , parenTest "a * (b + c)" "(a * (b + c))"
  , parenTest "((a))" "a"
  ]

nonAssoc :: [Test]
nonAssoc =
  [ failTest "a == b == c"
  , failTest "a != b != c"
  , failTest "a == b != c"
  , failTest "a < b < c"
  , failTest "a < b <= c"
  , failTest "a > b >= c"
  , failTest "a instanceof b instanceof c"
  , failTest "a < b instanceof c"
  , failTest "a in b in c"
  , failTest "a < b in c"
  , failTest "a in b instanceof c"
  ]

casts :: [Test]
casts =
  [ astTest "x as T" (castAs (iden "x") (tref "T"))
  , astTest "x satisfies T" (castSat (iden "x") (tref "T"))
  , astTest "x as const" (EAsConst (iden "x"))
  , astTest "[1, 2] as const" (EAsConst (EArr [ElemPlain (num "1"), ElemPlain (num "2")]))
  , astTest "x as A as B" (castAs (castAs (iden "x") (tref "A")) (tref "B"))
  , astTest "x as T satisfies U" (castSat (castAs (iden "x") (tref "T")) (tref "U"))
  , astTest "x as A | B" (castAs (iden "x") (TEBinOp TUnion (tref "A") (tref "B")))
  , astTest "x as T[]" (castAs (iden "x") (TEArr (tref "T")))
  , astTest "x as Map(K, V)" (castAs (iden "x") (TEApp [Ident "Map"] [tref "K", tref "V"]))
  , astTest "x as fn() => A" (castAs (iden "x") (TEFunc [] [] (tref "A")))
  , astTest "{ a: 1 } satisfies R" (castSat (EObj [OFKeyVal (key "a") (num "1")]) (tref "R"))

  , astTest "a + b as T" (castAs (EBinOp Add (iden "a") (iden "b")) (tref "T"))
  , astTest "a << b as T" (castAs (EBinOp Shl (iden "a") (iden "b")) (tref "T"))
  , astTest "a == b as T" (EBinOp Eq (iden "a") (castAs (iden "b") (tref "T")))
  , astTest "a < b as T" (EBinOp Lt (iden "a") (castAs (iden "b") (tref "T")))
  , astTest "-x as T" (castAs (EUnOp Neg (iden "x")) (tref "T"))
  , astTest "await p as T" (castAs (EUnOp Await (iden "p")) (tref "T"))
  , astTest "f(x) as T" (castAs (EApp (iden "f") (args [iden "x"])) (tref "T"))
  , astTest "(x as T).y" (EMem (castAs (iden "x") (tref "T")) (MNPub "y"))
  , astTest "x = y as T" (EAssign AEq (iden "x") (castAs (iden "y") (tref "T")))
  , astTest "f(x as T)" (EApp (iden "f") (args [castAs (iden "x") (tref "T")]))

  , failTest "x as"
  , failTest "x satisfies"
  , failTest "x satisfies const"
  , failTest "as T"
  , failTest "x as T y"
  ]

ins :: [Test]
ins =
  [ astTest "\"x\" in o" (EBinOp In (str "x") (iden "o"))
  , astTest "k in o.p" (EBinOp In (iden "k") (EMem (iden "o") (MNPub "p")))
  , astTest "a in b && c" (EBinOp And (EBinOp In (iden "a") (iden "b")) (iden "c"))
  , astTest "!(k in o)" (EUnOp Not (EBinOp In (iden "k") (iden "o")))
  , astTest "a + b in o" (EBinOp In (EBinOp Add (iden "a") (iden "b")) (iden "o"))
  , astTest "(k in o) == b" (EBinOp Eq (EBinOp In (iden "k") (iden "o")) (iden "b"))

  , astTest "#x in o" (EBinOp In (priv "x") (iden "o"))
  , astTest "#x in this" (EBinOp In (priv "x") (iden "this"))
  , astTest "fn(o) => #x in o" (lam [param "o"] (EBinOp In (priv "x") (iden "o")))

  , astTest "o.in" (EMem (iden "o") (MNPub "in"))
  , astTest "{ in: 1 }" (EObj [OFKeyVal (key "in") (num "1")])

  , failTest "#x.y in o"
  , failTest "in o"
  , failTest "a in"
  ]

deletes :: [Test]
deletes =
  [ astTest "delete x.y" (EUnOp Delete (EMem (iden "x") (MNPub "y")))
  , astTest "delete x[0]" (EUnOp Delete (EIndex (iden "x") (num "0")))
  , astTest "delete x?.y" (EUnOp Delete (EOptMem (iden "x") (MNPub "y")))
  , astTest "!delete x.y" (EUnOp Not (EUnOp Delete (EMem (iden "x") (MNPub "y"))))
  , astTest "delete a.b && c" (EBinOp And (EUnOp Delete (EMem (iden "a") (MNPub "b"))) (iden "c"))

  , failTest "delete"
  ]

lambdas :: [Test]
lambdas =
  [ astTest "fn() => x" (lam [] (iden "x"))
  , astTest "fn(x) => x" (lam [param "x"] (iden "x"))
  , astTest "fn(x, y) => x" (lam [param "x", param "y"] (iden "x"))
  , astTest "fn(x) => fn(y) => x" (lam [param "x"] (lam [param "y"] (iden "x")))

  , astTest "fn(x) => x + 1" (lam [param "x"] (EBinOp Add (iden "x") (num "1")))
  , astTest "fn(x) => x = 1" (lam [param "x"] (EAssign AEq (iden "x") (num "1")))
  , astTest "fn(x) => if a then b else c" (lam [param "x"] (EIfThen (iden "a") (iden "b") (iden "c")))
  , astTest "fn(x) => do { a; b }" (lam [param "x"] (EBlock Expl (BlockKey "do") [iden "a", iden "b"]))

  , astTest "(fn(x) => x)(1)" (EApp (lam [param "x"] (iden "x")) (args [num "1"]))
  , astTest "f(fn(x) => x, y)" (EApp (iden "f") (args [lam [param "x"] (iden "x"), iden "y"]))

  , astTest "fn(x: number) => x" (lam [paramT "x" (tref "number")] (iden "x"))
  , astTest "fn(x: A, y) => y" (lam [paramT "x" (tref "A"), param "y"] (iden "y"))
  , astTest "fn(x: A | B) => x" (lam [paramT "x" (TEBinOp TUnion (tref "A") (tref "B"))] (iden "x"))
  , astTest "fn(f: fn(x: A) => B) => f"
      (lam [paramT "f" (TEFunc [] [TFPIdent (Ident "x") False (tref "A")] (tref "B"))] (iden "f"))
  , astTest "fn(f: forall T. fn(x: T) => T) => f"
      (lam [paramT "f" (TEFunc [Ident "T"] [TFPIdent (Ident "x") False (tref "T")] (tref "T"))] (iden "f"))

  , astTest "fn() => 2" (lam [] (num "2"))

  , failTest "fn => x"
  , failTest "fn x => x"
  , failTest "fn(x) x"
  , failTest "fn(x) -> x"
  , failTest "fn(1) => x"
  , failTest "fn(x:) => x"
  , failTest "fn(x) =>"
  ]

params :: [Test]
params =
  [ astTest "fn(...xs) => xs" (lam [rest "xs"] (iden "xs"))
  , astTest "fn(a, ...xs) => a" (lam [param "a", rest "xs"] (iden "a"))
  , astTest "fn(a, b, ...xs) => a" (lam [param "a", param "b", rest "xs"] (iden "a"))
  , astTest "fn(...xs: number[]) => xs" (lam [LPRest (pv "xs") (Just (TEArr (tref "number")))] (iden "xs"))
  , astTest "fn(a: A, ...xs: A[]) => a" (lam [paramT "a" (tref "A"), LPRest (pv "xs") (Just (TEArr (tref "A")))] (iden "a"))

  , astTest "fn(x = 1) => x" (lam [paramD "x" (num "1")] (iden "x"))
  , astTest "fn(x, y = 2) => y" (lam [param "x", paramD "y" (num "2")] (iden "y"))
  , astTest "fn(x: number = 1) => x" (lam [LPPlain (pv "x") (Just (tref "number")) (Just (num "1"))] (iden "x"))
  , astTest "fn(x = a + b) => x" (lam [paramD "x" (EBinOp Add (iden "a") (iden "b"))] (iden "x"))
  , astTest "fn(f = fn() => 1) => f" (lam [paramD "f" (lam [] (num "1"))] (iden "f"))
  , astTest "fn(x = 1, ...ys) => x" (lam [paramD "x" (num "1"), rest "ys"] (iden "x"))
  , astTest "fn(...xs,) => xs" (lam [rest "xs"] (iden "xs"))

  , failTest "fn(...xs, a) => a"
  , failTest "fn(a ...xs) => a"
  , failTest "fn(...) => a"
  , failTest "fn(...xs = 1) => xs"
  , failTest "fn(...1) => a"
  , failTest "fn(x = ) => x"
  , failTest "fn(x: = 1) => x"
  ]

signatures :: [Test]
signatures =
  [ astTest "fn(x): number => x"
      (ELam (FuncSig False [] [param "x"] (Just (tref "number"))) (iden "x"))
  , astTest "fn(): A | B => x"
      (ELam (FuncSig False [] [] (Just (TEBinOp TUnion (tref "A") (tref "B")))) (iden "x"))
  , astTest "fn(): A[] => x"
      (ELam (FuncSig False [] [] (Just (TEArr (tref "A")))) (iden "x"))
  , astTest "fn(...xs: A[]): number => n"
      (ELam (FuncSig False [] [LPRest (pv "xs") (Just (TEArr (tref "A")))] (Just (tref "number"))) (iden "n"))
  , astTest "forall T. fn(x: T) => x"
      (ELam (FuncSig False [Ident "T"] [paramT "x" (tref "T")] Nothing) (iden "x"))
  , astTest "forall T. fn(x: T): T => x"
      (ELam (FuncSig False [Ident "T"] [paramT "x" (tref "T")] (Just (tref "T"))) (iden "x"))
  , astTest "forall K V. fn(m: Map(K, V)): V[] => m"
      (ELam (FuncSig False [Ident "K", Ident "V"]
                     [paramT "m" (TEApp [Ident "Map"] [tref "K", tref "V"])]
                     (Just (TEArr (tref "V"))))
            (iden "m"))

  , astTest "fn(): (fn() => A) => f"
      (ELam (FuncSig False [] [] (Just (TEFunc [] [] (tref "A")))) (iden "f"))
  , astTest "fn(): fn() => A => f"
      (ELam (FuncSig False [] [] (Just (TEFunc [] [] (tref "A")))) (iden "f"))

  , astTest "f(forall T. fn(x: T) => x)"
      (EApp (iden "f") (args [ELam (FuncSig False [Ident "T"] [paramT "x" (tref "T")] Nothing) (iden "x")]))
  , astTest "{ id: forall T. fn(x: T): T => x }"
      (EObj [OFKeyVal (key "id")
               (ELam (FuncSig False [Ident "T"] [paramT "x" (tref "T")] (Just (tref "T"))) (iden "x"))])

  , failTest "forall T. x"
  , failTest "forall. fn() => x"
  , failTest "forall T fn() => x"
  , failTest "forall 1. fn() => x"
  , failTest "forall T. forall U. fn() => x"
  , failTest "fn(x): => x"
  , failTest "fn(x): A"
  , failTest "fn(x) : A x"
  ]

asyncs :: [Test]
asyncs =
  [ astTest "async fn() => x" (alam [] (iden "x"))
  , astTest "async fn(x) => await f(x)"
      (alam [param "x"] (EUnOp Await (EApp (iden "f") (args [iden "x"]))))
  , astTest "async forall T. fn(x: T): Promise(T) => x"
      (ELam (FuncSig True [Ident "T"] [paramT "x" (tref "T")] (Just (TEApp [Ident "Promise"] [tref "T"]))) (iden "x"))
  , astTest "const f = async fn() => 1" (constD (pv "f") (alam [] (num "1")))
  , astTest "f(async fn() => 1)" (EApp (iden "f") (args [alam [] (num "1")]))

  , astTest "async" (iden "async")
  , astTest "async(x)" (EApp (iden "async") (args [iden "x"]))
  , astTest "async + 1" (EBinOp Add (iden "async") (num "1"))

  , astTest "class A where { async fn f() => 1 }"
      (cls "A" [CMethod [] (key "f") (FuncSig True [] [] Nothing) (Just (num "1"))])
  , astTest "class A where { static async fn f() => 1 }"
      (cls "A" [CMethod [MMStatic] (key "f") (FuncSig True [] [] Nothing) (Just (num "1"))])
  , astTest "class A where { async forall T. fn f(x: T) => x }"
      (cls "A" [CMethod [] (key "f") (FuncSig True [Ident "T"] [paramT "x" (tref "T")] Nothing) (Just (iden "x"))])
  , astTest "class A where { \"async\" = 1 }"
      (cls "A" [CMField [] (PKStr "async") False Nothing (Just (num "1"))])

  , failTest "async x"
  , failTest "async fn"
  , failTest "forall T. async fn() => x"
  , failTest "class A where { async = 1 }"
  ]

trailingCommas :: [Test]
trailingCommas =
  [ astTest "[1,]" (EArr [ElemPlain (num "1")])
  , astTest "[1, 2,]" (EArr [ElemPlain (num "1"), ElemPlain (num "2")])
  , astTest "[...xs,]" (EArr [ElemSpread (iden "xs")])
  , astTest "{ a: 1, }" (EObj [OFKeyVal (key "a") (num "1")])
  , astTest "{ a, }" (EObj [OFShort (Ident "a")])
  , astTest "f(a,)" (EApp (iden "f") (args [iden "a"]))
  , astTest "f(a, b,)" (EApp (iden "f") (args [iden "a", iden "b"]))
  , astTest "f(...xs,)" (EApp (iden "f") [ElemSpread (iden "xs")])
  , astTest "a?.(x,)" (EOptApp (iden "a") (args [iden "x"]))
  , astTest "new Foo(x,)" (ENew (iden "Foo") (args [iden "x"]))
  , astTest "fn(x,) => x" (lam [param "x"] (iden "x"))
  , astTest "fn(x, y = 1,) => x" (lam [param "x", paramD "y" (num "1")] (iden "x"))

  , failTest "[1,,]"
  , failTest "{ a: 1,, }"
  , failTest "f(,)"
  , failTest "f(a,,)"
  , failTest "fn(,) => x"
  , failTest "fn(x,,) => x"
  ]

conditionals :: [Test]
conditionals =
  [ astTest "if a then b else c" (EIfThen (iden "a") (iden "b") (iden "c"))
  , astTest "if a then b else if c then d else e"
      (EIfThen (iden "a") (iden "b") (EIfThen (iden "c") (iden "d") (iden "e")))
  , astTest "if a then if b then c else d else e"
      (EIfThen (iden "a") (EIfThen (iden "b") (iden "c") (iden "d")) (iden "e"))
  , astTest "if a == b then c else d" (EIfThen (EBinOp Eq (iden "a") (iden "b")) (iden "c") (iden "d"))

  , astTest "if a then b else c + d" (EIfThen (iden "a") (iden "b") (EBinOp Add (iden "c") (iden "d")))
  , astTest "(if a then b else c) + d" (EBinOp Add (EIfThen (iden "a") (iden "b") (iden "c")) (iden "d"))

  , astTest "if x = 1 then a else b" (EIfThen (EAssign AEq (iden "x") (num "1")) (iden "a") (iden "b"))

  , failTest "if a then b"
  , failTest "if a b else c"
  , failTest "if then a else b"
  , failTest "if a then else b"
  ]

tries :: [Test]
tries =
  [ astTest "try a catch e => b" (ETry (iden "a") (catch (Just "e") (iden "b")) Nothing)
  , astTest "try a catch => b" (ETry (iden "a") (catch Nothing (iden "b")) Nothing)
  , astTest "try a finally c" (ETry (iden "a") Nothing (Just (iden "c")))
  , astTest "try a catch e => b finally c"
      (ETry (iden "a") (catch (Just "e") (iden "b")) (Just (iden "c")))
  , astTest "try a catch e => b + 1"
      (ETry (iden "a") (catch (Just "e") (EBinOp Add (iden "b") (num "1"))) Nothing)
  , astTest "try f() catch e => throw e"
      (ETry (EApp (iden "f") []) (catch (Just "e") (EThrow (iden "e"))) Nothing)
  , astTest "x = try a catch e => b"
      (EAssign AEq (iden "x") (ETry (iden "a") (catch (Just "e") (iden "b")) Nothing))
  , astTest "const d = try JSON.parse(s) catch => null"
      (constD (pv "d")
        (ETry (EApp (EMem (iden "JSON") (MNPub "parse")) (args [iden "s"]))
              (catch Nothing (ELit (LNull "null"))) Nothing))
  , astTest "try do { a; b } catch e => do { c }"
      (ETry (EBlock Expl (BlockKey "do") [iden "a", iden "b"])
            (catch (Just "e") (EBlock Expl (BlockKey "do") [iden "c"])) Nothing)

  , astTest "p.catch(f)" (EApp (EMem (iden "p") (MNPub "catch")) (args [iden "f"]))
  , astTest "p.finally(g)" (EApp (EMem (iden "p") (MNPub "finally")) (args [iden "g"]))
  , astTest "class A where { fn catch(f) => f }" (cls "A" [meth "catch" [param "f"] (iden "f")])

  , failTest "try a"
  , failTest "try catch e => b"
  , failTest "try a catch e b"
  , failTest "try a catch 1 => b"
  , failTest "try a finally"
  , failTest "try a catch e => b catch f => c"
  , failTest "try a finally b catch e => c"
  , failTest "let try = 1"
  , failTest "let catch = 1"
  , failTest "let finally = 1"
  ]

contKeywords :: [Test]
contKeywords =
  [ astTest "if a; then b; else c" (EIfThen (iden "a") (iden "b") (iden "c"))
  , astTest "try a; catch e => b; finally c"
      (ETry (iden "a") (catch (Just "e") (iden "b")) (Just (iden "c")))
  , astTest "do { if a; then b; else c; d }"
      (EBlock Expl (BlockKey "do") [EIfThen (iden "a") (iden "b") (iden "c"), iden "d"])
  , astTest "do { try a; catch e => b; c }"
      (EBlock Expl (BlockKey "do") [ETry (iden "a") (catch (Just "e") (iden "b")) Nothing, iden "c"])
  , astTest "do { try a; finally b }"
      (EBlock Expl (BlockKey "do") [ETry (iden "a") Nothing (Just (iden "b"))])

  , failTest "do { a; then b }"
  , failTest "if a then b; c"
  , failTest "if a;; then b else c"
  ]

throws :: [Test]
throws =
  [ astTest "throw e" (EThrow (iden "e"))
  , astTest "throw new Error(\"x\")" (EThrow (ENew (iden "Error") (args [str "x"])))
  , astTest "throw a ?? b" (EThrow (EBinOp Coalesce (iden "a") (iden "b")))
  , astTest "fn() => throw e" (lam [] (EThrow (iden "e")))
  , astTest "if c then x else throw e" (EIfThen (iden "c") (iden "x") (EThrow (iden "e")))
  , astTest "a ?? (throw e)" (EBinOp Coalesce (iden "a") (EThrow (iden "e")))
  , astTest "do { throw e }" (EBlock Expl (BlockKey "do") [EThrow (iden "e")])
  , astTest "x = throw e" (EAssign AEq (iden "x") (EThrow (iden "e")))

  , failTest "throw"
  , failTest "a ?? throw e"
  ]

assignment :: [Test]
assignment =
  [ astTest ("x " <> assignop2str op <> " y") (EAssign op (iden "x") (iden "y")) | op <- [minBound ..] ]
  <>
  [ astTest "a.b = c" (EAssign AEq (EMem (iden "a") (MNPub "b")) (iden "c"))
  , astTest "a[0] = b" (EAssign AEq (EIndex (iden "a") (num "0")) (iden "b"))
  , astTest "x! = 1" (EAssign AEq (ENonNull (iden "x")) (num "1"))

  , astTest "a = b = c" (EAssign AEq (iden "a") (EAssign AEq (iden "b") (iden "c")))
  , astTest "a = b += 1" (EAssign AEq (iden "a") (EAssign AAdd (iden "b") (num "1")))
  , astTest "(a = b) = c" (EAssign AEq (EAssign AEq (iden "a") (iden "b")) (iden "c"))

  , astTest "x = a + b * c" (EAssign AEq (iden "x") (EBinOp Add (iden "a") (EBinOp Mul (iden "b") (iden "c"))))
  , astTest "x = fn() => 1" (EAssign AEq (iden "x") (lam [] (num "1")))
  , astTest "x = if a then b else c" (EAssign AEq (iden "x") (EIfThen (iden "a") (iden "b") (iden "c")))
  , astTest "x = a == b" (EAssign AEq (iden "x") (EBinOp Eq (iden "a") (iden "b")))

  , astTest "x == 1" (EBinOp Eq (iden "x") (num "1"))
  , astTest "x != 1" (EBinOp Neq (iden "x") (num "1"))
  , astTest "x <= 1" (EBinOp Lte (iden "x") (num "1"))
  , astTest "x >= 1" (EBinOp Gte (iden "x") (num "1"))

  , astTest "a + b" (EBinOp Add (iden "a") (iden "b"))
  , astTest "(x = 1)" (EAssign AEq (iden "x") (num "1"))

  , astTest "((((((((((((((((((((x))))))))))))))))))))" (iden "x")

  , astTest "[a, b] = [b, a]"
      (EAssign AEq (EArr [ElemPlain (iden "a"), ElemPlain (iden "b")]) (EArr [ElemPlain (iden "b"), ElemPlain (iden "a")]))

  , astTest "a + b = c" (EAssign AEq (EBinOp Add (iden "a") (iden "b")) (iden "c"))
  , astTest "f() = 1" (EAssign AEq (EApp (iden "f") []) (num "1"))
  , astTest "{ a, b } = o" (EAssign AEq (EObj [OFShort (Ident "a"), OFShort (Ident "b")]) (iden "o"))
  , astTest "({ a: x.y } = o)" (EAssign AEq (EObj [OFKeyVal (key "a") (EMem (iden "x") (MNPub "y"))]) (iden "o"))

  , failTest "x ="
  , failTest "= 1"
  , failTest "x += "
  , failTest "x = = 1"
  ]

declarations :: [Test]
declarations =
  [ astTest "let x = 1" (letD (pv "x") (num "1"))
  , astTest "const x = 1" (constD (pv "x") (num "1"))
  , astTest "let x = a + b" (letD (pv "x") (EBinOp Add (iden "a") (iden "b")))
  , astTest "const f = fn(x) => x" (constD (pv "f") (lam [param "x"] (iden "x")))
  , astTest "let x = if a then b else c" (letD (pv "x") (EIfThen (iden "a") (iden "b") (iden "c")))
  , astTest "let x = do { a; b }" (letD (pv "x") (EBlock Expl (BlockKey "do") [iden "a", iden "b"]))

  , astTest "const x = y = 1" (constD (pv "x") (EAssign AEq (iden "y") (num "1")))
  , astTest "let x = y += 1" (letD (pv "x") (EAssign AAdd (iden "y") (num "1")))

  , astTest "let [a, b] = xs" (letD (PArr [pel (pv "a"), pel (pv "b")]) (iden "xs"))
  , astTest "let [a, ...rest] = xs" (letD (PArr [pel (pv "a"), PERest (pv "rest")]) (iden "xs"))
  , astTest "let [a, [b, c]] = xs"
      (letD (PArr [pel (pv "a"), pel (PArr [pel (pv "b"), pel (pv "c")])]) (iden "xs"))
  , astTest "let [a = 1] = xs" (letD (PArr [PEPlain (pv "a") (Just (num "1"))]) (iden "xs"))
  , astTest "let {a, b} = o" (letD (PObj [pfs "a", pfs "b"]) (iden "o"))
  , astTest "let {a: x, ...o2} = o" (letD (PObj [pfk "a" (pv "x"), PFRest (Ident "o2")]) (iden "o"))
  , astTest "let {a: {b}} = o" (letD (PObj [pfk "a" (PObj [pfs "b"])]) (iden "o"))
  , astTest "let {a = 1} = o" (letD (PObj [PFShort (Ident "a") (Just (num "1"))]) (iden "o"))

  , failTest "let x"
  , failTest "const x"
  , failTest "let = 1"
  , failTest "let x = "
  , failTest "let x += 1"
  , failTest "let x.y = 1"
  , failTest "let f() = 1"
  , failTest "let [a + b] = xs"
  , failTest "let 1 = x"
  ]

annotatedDecls :: [Test]
annotatedDecls =
  [ astTest "let x: number = 1"
      (EDecl LetDecl (pv "x") (Just (tref "number")) (num "1"))
  , astTest "const x: A | null = y"
      (EDecl ConstDecl (pv "x") (Just (TEBinOp TUnion (tref "A") (TELit (LNull "null")))) (iden "y"))
  , astTest "const f: fn(x: A) => B = g"
      (EDecl ConstDecl (pv "f")
        (Just (TEFunc [] [TFPIdent (Ident "x") False (tref "A")] (tref "B"))) (iden "g"))
  , astTest "const id: forall T. fn(x: T) => T = fn(x) => x"
      (EDecl ConstDecl (pv "id")
        (Just (TEFunc [Ident "T"] [TFPIdent (Ident "x") False (tref "T")] (tref "T")))
        (lam [param "x"] (iden "x")))
  , astTest "let [a, b]: [A, B] = xs"
      (EDecl LetDecl (PArr [pel (pv "a"), pel (pv "b")])
        (Just (TETuple [TTEPlain (tref "A"), TTEPlain (tref "B")])) (iden "xs"))
  , astTest "let {a}: O = o"
      (EDecl LetDecl (PObj [pfs "a"]) (Just (tref "O")) (iden "o"))
  , astTest "const first = forall T. fn(xs: T[]): T | undefined => xs[0]"
      (constD (pv "first")
        (ELam (FuncSig False [Ident "T"] [paramT "xs" (TEArr (tref "T"))]
                       (Just (TEBinOp TUnion (tref "T") (tref "undefined"))))
              (EIndex (iden "xs") (num "0"))))

  , failTest "let x: = 1"
  , failTest "let x: A"
  , failTest "let x: A 1"
  , failTest "let x:: A = 1"
  ]

patterns :: [Test]
patterns =
  [ astTest "let [, b] = xs" (letD (PArr [PEHole, pel (pv "b")]) (iden "xs"))
  , astTest "let [a, , b] = xs" (letD (PArr [pel (pv "a"), PEHole, pel (pv "b")]) (iden "xs"))
  , astTest "let [,] = xs" (letD (PArr [PEHole]) (iden "xs"))
  , astTest "let [a,] = xs" (letD (PArr [pel (pv "a")]) (iden "xs"))
  , astTest "let [a,,] = xs" (letD (PArr [pel (pv "a"), PEHole]) (iden "xs"))
  , astTest "let [] = xs" (letD (PArr []) (iden "xs"))
  , astTest "let {} = o" (letD (PObj []) (iden "o"))

  , astTest "let [a, ...[b, c]] = xs"
      (letD (PArr [pel (pv "a"), PERest (PArr [pel (pv "b"), pel (pv "c")])]) (iden "xs"))
  , astTest "let [a, ...r,] = xs" (letD (PArr [pel (pv "a"), PERest (pv "r")]) (iden "xs"))
  , astTest "let { a, ...r, } = o" (letD (PObj [pfs "a", PFRest (Ident "r")]) (iden "o"))

  , astTest "let { a: b = 1 } = o" (letD (PObj [PFKey (key "a") (pv "b") (Just (num "1"))]) (iden "o"))
  , astTest "let { \"a-b\": x } = o" (letD (PObj [PFKey (PKStr "a-b") (pv "x") Nothing]) (iden "o"))
  , astTest "let { 0: x } = o" (letD (PObj [PFKey (PKNum "0") (pv "x") Nothing]) (iden "o"))
  , astTest "let { [k]: v } = o" (letD (PObj [PFKey (PKComp (iden "k")) (pv "v") Nothing]) (iden "o"))
  , astTest "let { default: d } = m" (letD (PObj [pfk "default" (pv "d")]) (iden "m"))
  , astTest "let { a: [b, { c }] } = o"
      (letD (PObj [pfk "a" (PArr [pel (pv "b"), pel (PObj [pfs "c"])])]) (iden "o"))
  , astTest "let [a = f(x), b = a] = xs"
      (letD (PArr [ PEPlain (pv "a") (Just (EApp (iden "f") (args [iden "x"])))
                  , PEPlain (pv "b") (Just (iden "a")) ]) (iden "xs"))

  , astTest "fn({ a, b }) => a" (lam [paramP (PObj [pfs "a", pfs "b"])] (iden "a"))
  , astTest "fn([x, y]) => x" (lam [paramP (PArr [pel (pv "x"), pel (pv "y")])] (iden "x"))
  , astTest "fn({ a, b }: Props) => a"
      (lam [LPPlain (PObj [pfs "a", pfs "b"]) (Just (tref "Props")) Nothing] (iden "a"))
  , astTest "fn({ a = 1 } = {}) => a"
      (lam [LPPlain (PObj [PFShort (Ident "a") (Just (num "1"))]) Nothing (Just (EObj []))] (iden "a"))
  , astTest "fn([first, ...others]) => first"
      (lam [paramP (PArr [pel (pv "first"), PERest (pv "others")])] (iden "first"))
  , astTest "fn(...[a, b]) => a" (lam [LPRest (PArr [pel (pv "a"), pel (pv "b")]) Nothing] (iden "a"))
  , astTest "fn(x, { y }) => y" (lam [param "x", paramP (PObj [pfs "y"])] (iden "y"))
  , astTest "fn({ a: { b } }) => b" (lam [paramP (PObj [pfk "a" (PObj [pfs "b"])])] (iden "b"))
  , astTest "class A where { fn f({ x, y }) => x }"
      (cls "A" [meth "f" [paramP (PObj [pfs "x", pfs "y"])] (iden "x")])
  , astTest "try a catch { message } => message"
      (ETry (iden "a") (Just (TryCatch (Just (PObj [pfs "message"])) (iden "message"))) Nothing)

  , failTest "let [...a, b] = xs"        -- rest must be last
  , failTest "let [a,, ...b, c] = xs"
  , failTest "let [...a = 1] = xs"       -- rest takes no default
  , failTest "let [...] = xs"
  , failTest "let { ...a, b } = o"
  , failTest "let { ...{ a } } = o"      -- object rest must be an identifier
  , failTest "let { a: 1 } = o"
  , failTest "let { if } = o"            -- keywords can't be shorthand bindings
  , failTest "let { #a } = o"
  , failTest "let [a b] = xs"
  , failTest "fn({ a + b }) => a"
  , failTest "fn([1]) => x"
  , failTest "fn({ a }:) => a"
  ]

keywordBindings :: [Test]
keywordBindings =
  [ failTest "let static = 1"
  , failTest "let protected = 1"
  , failTest "let extends = 1"
  , failTest "let implements = 1"
  , failTest "let class = 1"
  , failTest "let delete = 1"
  , failTest "let throw = 1"
  , failTest "let in = 1"
  , failTest "let import = 1"
  , failTest "let export = 1"
  , failTest "let default = 1"
  , failTest "fn(static) => 1"
  , failTest "let type = 1"
  , failTest "let fn = 1"

  , astTest "let readonly = 1" (letD (pv "readonly") (num "1"))
  , astTest "let override = 1" (letD (pv "override") (num "1"))
  , astTest "let abstract = 1" (letD (pv "abstract") (num "1"))
  , astTest "let async = 1" (letD (pv "async") (num "1"))
  , astTest "abstract + 1" (EBinOp Add (iden "abstract") (num "1"))
  , astTest "abstract()" (EApp (iden "abstract") [])
  , astTest "abstract.x" (EMem (iden "abstract") (MNPub "x"))
  , astTest "f(abstract)" (EApp (iden "f") (args [iden "abstract"]))

  , astTest "express.static(dir)" (EApp (EMem (iden "express") (MNPub "static")) (args [iden "dir"]))
  , astTest "a.extends" (EMem (iden "a") (MNPub "extends"))
  , astTest "m.delete(k)" (EApp (EMem (iden "m") (MNPub "delete")) (args [iden "k"]))
  , astTest "o.default" (EMem (iden "o") (MNPub "default"))
  , astTest "{ static: 1, class: 2 }" (EObj [OFKeyVal (key "static") (num "1"), OFKeyVal (key "class") (num "2")])
  , astTest "{ readonly: true }" (EObj [OFKeyVal (key "readonly") (ELit (LBool "true"))])
  ]

typeDecls :: [Test]
typeDecls =
  [ astTest "type Id = number" (ETypeDecl (Ident "Id") [] (tref "number"))
  , astTest "type U = A | B" (ETypeDecl (Ident "U") [] (TEBinOp TUnion (tref "A") (tref "B")))
  , astTest "type Pair(A, B) = [A, B]"
      (ETypeDecl (Ident "Pair") [Ident "A", Ident "B"] (TETuple [TTEPlain (tref "A"), TTEPlain (tref "B")]))
  , astTest "type Pair(A, B,) = [A, B]"
      (ETypeDecl (Ident "Pair") [Ident "A", Ident "B"] (TETuple [TTEPlain (tref "A"), TTEPlain (tref "B")]))
  , astTest "type O = { a: number, b?: string }"
      (ETypeDecl (Ident "O") [] (TEObj [TOFKeyVal (key "a") False (tref "number"), TOFKeyVal (key "b") True (tref "string")]))
  , astTest "type F(T) = fn(x: T) => T"
      (ETypeDecl (Ident "F") [Ident "T"] (TEFunc [] [TFPIdent (Ident "x") False (tref "T")] (tref "T")))
  , astTest "type F = fn() => 2" (ETypeDecl (Ident "F") [] (TEFunc [] [] (TELit (LNum "2"))))
  , astTest "type Id = forall T. fn(x: T) => T"
      (ETypeDecl (Ident "Id") [] (TEFunc [Ident "T"] [TFPIdent (Ident "x") False (tref "T")] (tref "T")))
  , astTest "type Opts = raw \"Partial<User>\"" (ETypeDecl (Ident "Opts") [] (TERaw (Raw "Partial<User>")))

  , failTest "type = number"
  , failTest "type A number"
  , failTest "type A ="
  , failTest "type a.b = C"
  , failTest "type A(x.y) = B"
  , failTest "type A(1) = B"
  , failTest "type P(,) = A"
  ]

classes :: [Test]
classes =
  [ astTest "class A where {}" (cls "A" [])
  , astTest "class A where { x }" (cls "A" [CMField [] (key "x") False Nothing Nothing])
  , astTest "class A where { x = 1 }" (cls "A" [fieldI "x" (num "1")])
  , astTest "class A where { x: number }" (cls "A" [CMField [] (key "x") False (Just (tref "number")) Nothing])
  , astTest "class A where { x?: number }" (cls "A" [CMField [] (key "x") True (Just (tref "number")) Nothing])
  , astTest "class A where { x: number = 1; y = 2 }"
      (cls "A" [CMField [] (key "x") False (Just (tref "number")) (Just (num "1")), fieldI "y" (num "2")])
  , astTest "class A where { x = 1; }" (cls "A" [fieldI "x" (num "1")])
  , astTest "class A where { #x = 1 }" (cls "A" [CMField [] (PKMem (MNPriv "x")) False Nothing (Just (num "1"))])
  , astTest "class A where { \"a-b\" = 1; 0 = 2 }"
      (cls "A" [ CMField [] (PKStr "a-b") False Nothing (Just (num "1"))
               , CMField [] (PKNum "0") False Nothing (Just (num "2")) ])
  , astTest "class A where { onClick = fn(e) => e }" (cls "A" [fieldI "onClick" (lam [param "e"] (iden "e"))])
  , astTest "class A where { type = \"a\" }" (cls "A" [fieldI "type" (str "a")])

  , astTest "class A where { fn f() => 1 }" (cls "A" [meth "f" [] (num "1")])
  , astTest "class A where { fn f(x: A): B => x }"
      (cls "A" [CMethod [] (key "f") (FuncSig False [] [paramT "x" (tref "A")] (Just (tref "B"))) (Just (iden "x"))])
  , astTest "class A where { fn constructor(x) => this.x = x }"
      (cls "A" [meth "constructor" [param "x"] (EAssign AEq (EMem (iden "this") (MNPub "x")) (iden "x"))])
  , astTest "class A where { forall T. fn id(x: T): T => x }"
      (cls "A" [CMethod [] (key "id") (FuncSig False [Ident "T"] [paramT "x" (tref "T")] (Just (tref "T"))) (Just (iden "x"))])
  , astTest "class A where { fn #helper() => 1 }" (cls "A" [CMethod [] (PKMem (MNPriv "helper")) sig0 (Just (num "1"))])
  , astTest "class A where { fn [k]() => 1 }" (cls "A" [CMethod [] (PKComp (iden "k")) sig0 (Just (num "1"))])
  , astTest "class A where { fn f(...xs) => xs }" (cls "A" [meth "f" [rest "xs"] (iden "xs")])
  , astTest "class A where { fn delete(id) => id }" (cls "A" [meth "delete" [param "id"] (iden "id")])
  , astTest "class A where { fn #check(o) => #x in o }"
      (cls "A" [CMethod [] (PKMem (MNPriv "check")) (FuncSig False [] [param "o"] Nothing)
                  (Just (EBinOp In (priv "x") (iden "o")))])

  , astTest "class A where { static count = 0 }"
      (cls "A" [CMField [MMStatic] (key "count") False Nothing (Just (num "0"))])
  , astTest "class A where { static readonly max = 10 }"
      (cls "A" [CMField [MMStatic, MMReadonly] (key "max") False Nothing (Just (num "10"))])
  , astTest "class A where { protected override fn f() => 1 }"
      (cls "A" [CMethod [MMProtected, MMOverride] (key "f") sig0 (Just (num "1"))])
  , astTest "class A where { static #n = 0 }"
      (cls "A" [CMField [MMStatic] (PKMem (MNPriv "n")) False Nothing (Just (num "0"))])
  , astTest "abstract class A where { abstract fn f(): number }"
      (cdecl True (Just "A") [] Nothing []
        [CMethod [MMAbstract] (key "f") (FuncSig False [] [] (Just (tref "number"))) Nothing])

  , astTest "class B extends A where {}" (cdecl False (Just "B") [] (Just (tref "A")) [] [])
  , astTest "class Box(T) extends Base(T) implements I, J(T) where {}"
      (cdecl False (Just "Box") ["T"]
        (Just (TEApp [Ident "Base"] [tref "T"]))
        [tref "I", TEApp [Ident "J"] [tref "T"]]
        [])
  , astTest "class Box(T,) where {}" (cdecl False (Just "Box") ["T"] Nothing [] [])
  , astTest "class where {}" (cdecl False Nothing [] Nothing [] [])
  , astTest "class extends A where {}" (cdecl False Nothing [] (Just (tref "A")) [] [])
  , astTest "const A = class where { x = 1 }"
      (constD (pv "A") (cdecl False Nothing [] Nothing [] [fieldI "x" (num "1")]))
  , astTest "class A where { fn f(): number }"
      (cls "A" [CMethod [] (key "f") (FuncSig False [] [] (Just (tref "number"))) Nothing])

  , failTest "class A"
  , failTest "class A where { x y }"
  , failTest "class A where { fn f }"
  , failTest "class A where { fn f() => }"
  , failTest "class A where { forall T. x = 1 }"
  , failTest "class A(1) where {}"
  , failTest "class A extends where {}"
  , failTest "class A implements where {}"
  , failTest "class A where { x: }"
  , failTest "class A where { x = }"
  , failTest "class A where { x = 1"
  ]

reservedMembers :: [Test]
reservedMembers =
  [ failTest "class A where { static = 1 }"
  , failTest "class A where { static }"
  , failTest "class A where { readonly: boolean }"
  , failTest "class A where { static readonly }"
  , failTest "class A where { abstract }"
  , failTest "class A where { override: number }"
  , failTest "class A where { fn = 1 }"
  , failTest "class A where { forall = 1 }"
  , failTest "class A where { fn static() => 1 }"

  , astTest "class A where { \"static\" = 1 }"
      (cls "A" [CMField [] (PKStr "static") False Nothing (Just (num "1"))])
  , astTest "class A where { \"forall\" = 1 }"
      (cls "A" [CMField [] (PKStr "forall") False Nothing (Just (num "1"))])
  , astTest "class A where { fn \"fn\"() => 1 }"
      (cls "A" [CMethod [] (PKStr "fn") sig0 (Just (num "1"))])
  , astTest "class A where { static \"readonly\": boolean }"
      (cls "A" [CMField [MMStatic] (PKStr "readonly") False (Just (tref "boolean")) Nothing])
  ]

imports :: [Test]
imports =
  [ astTest "import \"m\"" (EImport ImpBare "m")
  , astTest "import { a } from \"m\"" (EImport (ImpNamed [imp "a"]) "m")
  , astTest "import { a, b as c } from \"m\"" (EImport (ImpNamed [imp "a", impAs "b" "c"]) "m")
  , astTest "import { type T, a } from \"m\"" (EImport (ImpNamed [ImportSpec True "T" Nothing, imp "a"]) "m")
  , astTest "import { type T as U } from \"m\"" (EImport (ImpNamed [ImportSpec True "T" (Just (Ident "U"))]) "m")
  , astTest "import { default as React } from \"react\"" (EImport (ImpNamed [impAs "default" "React"]) "react")
  , astTest "import { delete as del } from \"m\"" (EImport (ImpNamed [impAs "delete" "del"]) "m")
  , astTest "import * as ns from \"m\"" (EImport (ImpNamespace (Ident "ns")) "m")
  , astTest "import {} from \"m\"" (EImport (ImpNamed []) "m")
  , astTest "import { a, } from \"m\"" (EImport (ImpNamed [imp "a"]) "m")

  , astTest "import(\"m\")" (EImportCall (args [str "m"]))
  , astTest "import(\"m\").then(f)"
      (EApp (EMem (EImportCall (args [str "m"])) (MNPub "then")) (args [iden "f"]))
  , astTest "await import(\"m\")" (EUnOp Await (EImportCall (args [str "m"])))
  , astTest "import.meta" EImportMeta
  , astTest "import.meta.url" (EMem EImportMeta (MNPub "url"))

  , astTest "from" (iden "from")
  , astTest "let from = 1" (letD (pv "from") (num "1"))

  , failTest "import x from \"m\""           -- no default imports
  , failTest "import type { T } from \"m\""  -- use inline `type`
  , failTest "import { default } from \"m\"" -- keyword names need an alias
  , failTest "import { a as default } from \"m\""
  , failTest "import { a } \"m\""
  , failTest "import { a } from m"
  , failTest "import * from \"m\""
  , failTest "import * as ns"
  , failTest "import"
  , failTest "import.foo"
  ]

exports :: [Test]
exports =
  [ astTest "export const x = 1" (EExportDecl (constD (pv "x") (num "1")))
  , astTest "export let x = 1" (EExportDecl (letD (pv "x") (num "1")))
  , astTest "export type T = A" (EExportDecl (ETypeDecl (Ident "T") [] (tref "A")))
  , astTest "export class A where {}" (EExportDecl (cls "A" []))
  , astTest "export abstract class A where {}" (EExportDecl (cdecl True (Just "A") [] Nothing [] []))
  , astTest "export const f = async fn() => 1" (EExportDecl (constD (pv "f") (alam [] (num "1"))))
  , astTest "export const { a, b } = o" (EExportDecl (constD (PObj [pfs "a", pfs "b"]) (iden "o")))

  , astTest "export { a }" (EExport (ExpNamed [expo "a"]) Nothing)
  , astTest "export { a, b as c }" (EExport (ExpNamed [expo "a", expoAs "b" "c"]) Nothing)
  , astTest "export { x as default }" (EExport (ExpNamed [expoAs "x" "default"]) Nothing)
  , astTest "export { type T }" (EExport (ExpNamed [ExportSpec True "T" Nothing]) Nothing)
  , astTest "export { a, b as c } from \"m\"" (EExport (ExpNamed [expo "a", expoAs "b" "c"]) (Just "m"))
  , astTest "export { default as React } from \"react\"" (EExport (ExpNamed [expoAs "default" "React"]) (Just "react"))
  , astTest "export * from \"m\"" (EExport ExpAll (Just "m"))
  , astTest "export * as ns from \"m\"" (EExport (ExpAllAs "ns") (Just "m"))

  , failTest "export default x"   -- use `export { x as default }`
  , failTest "export type { T }"  -- use inline `type`
  , failTest "export x"
  , failTest "export 1"
  , failTest "export fn f() => 1"
  , failTest "export *"
  , failTest "export * as ns"
  , failTest "export"
  ]

blocks :: [Test]
blocks =
  [ astTest "do {}" (EBlock Expl (BlockKey "do") [])
  , astTest "do { a }" (EBlock Expl (BlockKey "do") [iden "a"])
  , astTest "do { a; b }" (EBlock Expl (BlockKey "do") [iden "a", iden "b"])
  , astTest "do { a; b; c }" (EBlock Expl (BlockKey "do") [iden "a", iden "b", iden "c"])
  , astTest "do { a; }" (EBlock Expl (BlockKey "do") [iden "a"])
  , astTest "do { let x = 1; x }" (EBlock Expl (BlockKey "do") [letD (pv "x") (num "1"), iden "x"])
  , astTest "do { x = 1 }" (EBlock Expl (BlockKey "do") [EAssign AEq (iden "x") (num "1")])
  , astTest "do { do { a } }" (EBlock Expl (BlockKey "do") [EBlock Expl (BlockKey "do") [iden "a"]])
  , astTest "where { a }" (EBlock Expl (BlockKey "where") [iden "a"])
  , astTest "where { a; b }" (EBlock Expl (BlockKey "where") [iden "a", iden "b"])

  , failTest "do { a b }"
  , failTest "do a"
  , failTest "do { a"
  , failTest "do }"
  ]

loops :: [Test]
loops =
  [ astTest "for x of xs do { f(x) }"
      (EFor False (pv "x") (iden "xs") (dob [EApp (iden "f") (args [iden "x"])]))
  , astTest "for await c of s do { f(c) }"
      (EFor True (pv "c") (iden "s") (dob [EApp (iden "f") (args [iden "c"])]))
  , astTest "for [k, v] of Object.entries(o) do { f(k, v) }"
      (EFor False (PArr [pel (pv "k"), pel (pv "v")])
        (EApp (EMem (iden "Object") (MNPub "entries")) (args [iden "o"]))
        (dob [EApp (iden "f") (args [iden "k", iden "v"])]))
  , astTest "for { a } of xs do {}" (EFor False (PObj [pfs "a"]) (iden "xs") (dob []))
  , astTest "for x of xs.filter(p) do { f(x) }"
      (EFor False (pv "x") (EApp (EMem (iden "xs") (MNPub "filter")) (args [iden "p"]))
        (dob [EApp (iden "f") (args [iden "x"])]))

  , astTest "while x < 10 do { x += 1 }"
      (EWhile (EBinOp Lt (iden "x") (num "10")) (dob [EAssign AAdd (iden "x") (num "1")]))
  , astTest "while true do { break }" (EWhile (ELit (LBool "true")) (dob [EBreak]))
  , astTest "for x of xs do { if p(x) then continue else f(x) }"
      (EFor False (pv "x") (iden "xs")
        (dob [EIfThen (EApp (iden "p") (args [iden "x"])) EContinue (EApp (iden "f") (args [iden "x"]))]))

  , astTest "return" (EReturn Nothing)
  , astTest "return x + 1" (EReturn (Just (EBinOp Add (iden "x") (num "1"))))
  , astTest "if c then return else x" (EIfThen (iden "c") (EReturn Nothing) (iden "x"))
  , astTest "fn(x) => do { if x then return 1 else 2 }"
      (lam [param "x"] (dob [EIfThen (iden "x") (EReturn (Just (num "1"))) (num "2")]))
  , astTest "do { return }" (dob [EReturn Nothing])
  , astTest "a ?? (return)" (EBinOp Coalesce (iden "a") (EReturn Nothing))

  , astTest "it.return(x)" (EApp (EMem (iden "it") (MNPub "return")) (args [iden "x"]))
  , astTest "{ for: 1, while: 2 }" (EObj [OFKeyVal (key "for") (num "1"), OFKeyVal (key "while") (num "2")])
  , astTest "of" (iden "of")
  , astTest "let of = 1" (letD (pv "of") (num "1"))

  , failTest "for x in xs do {}"          -- no for in
  , failTest "for (let i = 0; i < n; i += 1) do {}"
  , failTest "for x of xs"                -- body required
  , failTest "for x of xs { f(x) }"       -- `do` required
  , failTest "for x of xs where { f(x) }"
  , failTest "for x xs do {}"
  , failTest "for 1 of xs do {}"
  , failTest "while x { f() }"
  , failTest "while do {}"
  , failTest "a ?? return"
  , failTest "let for = 1"
  , failTest "let while = 1"
  , failTest "let return = 1"
  , failTest "let break = 1"
  , failTest "let continue = 1"
  ]

interfaces :: [Test]
interfaces =
  [ astTest "interface A where {}" (EInterface (Ident "A") [] [] [])
  , astTest "interface Point where { x: number; y: number }"
      (EInterface (Ident "Point") [] [] [ifld "x" (tref "number"), ifld "y" (tref "number")])
  , astTest "interface A where { x?: number }"
      (EInterface (Ident "A") [] [] [InterfaceField False (key "x") True (tref "number")])
  , astTest "interface A where { readonly id: string }"
      (EInterface (Ident "A") [] [] [InterfaceField True (key "id") False (tref "string")])
  , astTest "interface A where { readonly x?: T }"
      (EInterface (Ident "A") [] [] [InterfaceField True (key "x") True (tref "T")])
  , astTest "interface Box(T) where { value: T }"
      (EInterface (Ident "Box") [Ident "T"] [] [ifld "value" (tref "T")])
  , astTest "interface B extends A where {}" (EInterface (Ident "B") [] [tref "A"] [])
  , astTest "interface C(T) extends A, B(T) where {}"
      (EInterface (Ident "C") [Ident "T"] [tref "A", TEApp [Ident "B"] [tref "T"]] [])
  , astTest "interface A where { f: fn(x: number) => string }"
      (EInterface (Ident "A") [] []
        [ifld "f" (TEFunc [] [TFPIdent (Ident "x") False (tref "number")] (tref "string"))])
  , astTest "interface A where { type: \"click\" }"
      (EInterface (Ident "A") [] [] [ifld "type" (TELit (LStr "click"))])
  , astTest "interface A where { \"a-b\": number; 0: string }"
      (EInterface (Ident "A") [] []
        [ InterfaceField False (PKStr "a-b") False (tref "number")
        , InterfaceField False (PKNum "0") False (tref "string") ])
  , astTest "interface A where { \"readonly\": boolean }"
      (EInterface (Ident "A") [] [] [InterfaceField False (PKStr "readonly") False (tref "boolean")])
  , astTest "interface A where { x: number; }"
      (EInterface (Ident "A") [] [] [ifld "x" (tref "number")])
  , astTest "export interface A where { x: number }"
      (EExportDecl (EInterface (Ident "A") [] [] [ifld "x" (tref "number")]))

  , failTest "interface A"
  , failTest "interface where {}"
  , failTest "interface A where { x }"                    -- type required
  , failTest "interface A where { readonly: boolean }"    -- reserved; quote it
  , failTest "interface A where { x: number, y: number }" -- fields aren't comma-separated
  , failTest "interface A where { #x: number }"
  , failTest "interface A where { fn f(): number }"       -- no method signatures
  , failTest "interface A extends where {}"
  , failTest "let interface = 1"
  ]

grouping :: [Test]
grouping =
  [ astTest "(a)" (iden "a")
  , astTest "((a))" (iden "a")
  , astTest "(a + b)" (EBinOp Add (iden "a") (iden "b"))

  , failTest "()"
  , failTest "(a"
  , failTest "(a, b)"
  , failTest "(a,)"
  ]

topLevel :: [Test]
topLevel =
  [ astTop "" []
  , astTop "a" [iden "a"]
  , astTop "a\nb\nc" [iden "a", iden "b", iden "c"]
  , astTop "a;b" [iden "a", iden "b"]
  , astTop "a;" [iden "a"]
  , astTop "let x = 1" [letD (pv "x") (num "1")]
  , astTop "if a then b else c" [EIfThen (iden "a") (iden "b") (iden "c")]

  , astTop "do\n  a\n  b" [EBlock Impl (BlockKey "do") [iden "a", iden "b"]]
  , astTop "where\n  a\n  b" [EBlock Impl (BlockKey "where") [iden "a", iden "b"]]
  , astTop "do\n  a;b" [EBlock Impl (BlockKey "do") [iden "a", iden "b"]]
  , astTop "do\n  a\n  do\n    b" [EBlock Impl (BlockKey "do") [iden "a", EBlock Impl (BlockKey "do") [iden "b"]]]
  , astTop "do\n  a\n  do\n    b\n  c"
      [EBlock Impl (BlockKey "do") [iden "a", EBlock Impl (BlockKey "do") [iden "b"], iden "c"]]
  , astTop "do\n  a\nb" [EBlock Impl (BlockKey "do") [iden "a"], iden "b"]
  , astTop "x\ndo\ny" [iden "x", EBlock Impl (BlockKey "do") [], iden "y"]
  , astTop "do { a; b }" [EBlock Expl (BlockKey "do") [iden "a", iden "b"]]
  , astTop "do\n  x += 1\n  x" [EBlock Impl (BlockKey "do") [EAssign AAdd (iden "x") (num "1"), iden "x"]]
  , astTop "x = do\n  a\n  b" [EAssign AEq (iden "x") (EBlock Impl (BlockKey "do") [iden "a", iden "b"])]

  , astTop "let x =\n  1" [letD (pv "x") (num "1")]
  , astTop "a\n  + b" [EBinOp Add (iden "a") (iden "b")]
  , astTop "if a\n  then b\n  else c" [EIfThen (iden "a") (iden "b") (iden "c")]

  , astTop "let xs = [\n  1,\n  2\n]" [letD (pv "xs") (EArr [ElemPlain (num "1"), ElemPlain (num "2")])]
  , astTop "f(\n  a,\n  b\n)" [EApp (iden "f") (args [iden "a", iden "b"])]
  , astTop "{\n  a: 1,\n  b: 2\n}" [EObj [OFKeyVal (key "a") (num "1"), OFKeyVal (key "b") (num "2")]]

  , astTop "let xs = [\n  1,\n  2,\n]" [letD (pv "xs") (EArr [ElemPlain (num "1"), ElemPlain (num "2")])]
  , astTop "f(\n  a,\n  b,\n)" [EApp (iden "f") (args [iden "a", iden "b"])]
  , astTop "{\n  a: 1,\n  b: 2,\n}" [EObj [OFKeyVal (key "a") (num "1"), OFKeyVal (key "b") (num "2")]]
  , astTop "const f = fn(\n  a,\n  ...rest\n) => a" [constD (pv "f") (lam [param "a", rest "rest"] (iden "a"))]

  , astTop "const first = forall T.\n  fn(xs: T[]): T | undefined => xs[0]"
      [constD (pv "first")
        (ELam (FuncSig False [Ident "T"] [paramT "xs" (TEArr (tref "T"))]
                       (Just (TEBinOp TUnion (tref "T") (tref "undefined"))))
              (EIndex (iden "xs") (num "0")))]

  , astTop "type Color = \"red\"\n  | \"green\""
      [ETypeDecl (Ident "Color") [] (TEBinOp TUnion (TELit (LStr "red")) (TELit (LStr "green")))]
  , astTop "type A = number\nlet x = 1" [ETypeDecl (Ident "A") [] (tref "number"), letD (pv "x") (num "1")]

  , astTop "type Point = { x: number, y: number }\nlet p = { x: 1, y: 2 }\nlet dist = fn(a: Point) => do\n  let dx = a.x\n  dx * dx"
      [ ETypeDecl (Ident "Point") []
          (TEObj [TOFKeyVal (key "x") False (tref "number"), TOFKeyVal (key "y") False (tref "number")])
      , letD (pv "p") (EObj [OFKeyVal (key "x") (num "1"), OFKeyVal (key "y") (num "2")])
      , letD (pv "dist")
          (lam [paramT "a" (tref "Point")]
            (EBlock Impl (BlockKey "do")
              [ letD (pv "dx") (EMem (iden "a") (MNPub "x"))
              , EBinOp Mul (iden "dx") (iden "dx") ]))
      ]

  , astTop "const load = async fn(url) => do\n  const res = await fetch(url)\n  if res.ok then res.json() else throw new Error(\"failed\")"
      [constD (pv "load")
        (alam [param "url"]
          (EBlock Impl (BlockKey "do")
            [ constD (pv "res") (EUnOp Await (EApp (iden "fetch") (args [iden "url"])))
            , EIfThen (EMem (iden "res") (MNPub "ok"))
                (EApp (EMem (iden "res") (MNPub "json")) [])
                (EThrow (ENew (iden "Error") (args [str "failed"])))
            ]))]
  , astTop "const cfg = {\n  a: 1,\n} as const" [constD (pv "cfg") (EAsConst (EObj [OFKeyVal (key "a") (num "1")]))]
  , astTop "x\n  as T" [castAs (iden "x") (tref "T")]

  , astTop "class A where\n  x = 1\n  fn f() => x"
      [cls "A" [fieldI "x" (num "1"), meth "f" [] (iden "x")]]
  , astTop "class B extends A where\n  fn constructor() => do\n    super()\n    this.x = 1\n  fn g() => 2"
      [cdecl False (Just "B") [] (Just (tref "A")) []
        [ meth "constructor" []
            (EBlock Impl (BlockKey "do")
              [ EApp (iden "super") []
              , EAssign AEq (EMem (iden "this") (MNPub "x")) (num "1") ])
        , meth "g" [] (num "2")
        ]]
  , astTop "class A where\n  static count = 0\n\n  #items: number[] = []\n  fn push(x) => this.#items.push(x)"
      [cls "A"
        [ CMField [MMStatic] (key "count") False Nothing (Just (num "0"))
        , CMField [] (PKMem (MNPriv "items")) False (Just (TEArr (tref "number"))) (Just (EArr []))
        , meth "push" [param "x"]
            (EApp (EMem (EMem (iden "this") (MNPriv "items")) (MNPub "push")) (args [iden "x"]))
        ]]
  , astTop "class E where\nx" [cls "E" [], iden "x"]

  , astTop "import {\n  a,\n  type T,\n} from \"m\"\nimport * as ns from \"n\"\n\nexport const f = fn(x: T) => ns.g(a, x)"
      [ EImport (ImpNamed [imp "a", ImportSpec True "T" Nothing]) "m"
      , EImport (ImpNamespace (Ident "ns")) "n"
      , EExportDecl (constD (pv "f")
          (lam [paramT "x" (tref "T")] (EApp (EMem (iden "ns") (MNPub "g")) (args [iden "a", iden "x"]))))
      ]
  , astTop "export class A where\n  x = 1\n  fn f() => x"
      [EExportDecl (cls "A" [fieldI "x" (num "1"), meth "f" [] (iden "x")])]
  , astTop "const App = fn() => 1\nexport { App as default }"
      [ constD (pv "App") (lam [] (num "1"))
      , EExport (ExpNamed [expoAs "App" "default"]) Nothing
      ]

  , astTop "do\n  if a\n  then b\n  else c"
      [EBlock Impl (BlockKey "do") [EIfThen (iden "a") (iden "b") (iden "c")]]
  , astTop "if a\nthen b\nelse c" [EIfThen (iden "a") (iden "b") (iden "c")]
  , astTop "const data = try do\n    JSON.parse(s)\n  catch e => null"
      [constD (pv "data")
        (ETry (EBlock Impl (BlockKey "do") [EApp (EMem (iden "JSON") (MNPub "parse")) (args [iden "s"])])
              (catch (Just "e") (ELit (LNull "null"))) Nothing)]
  , astTop "do\n  try\n    risky()\n  catch e => log(e)\n  finally done()\n  next()"
      [EBlock Impl (BlockKey "do")
        [ ETry (EApp (iden "risky") [])
               (catch (Just "e") (EApp (iden "log") (args [iden "e"])))
               (Just (EApp (iden "done") []))
        , EApp (iden "next") []
        ]]

  , astTop "const Card = fn({\n  title,\n  body = \"\",\n}: Props) => body"
      [constD (pv "Card")
        (lam [LPPlain (PObj [pfs "title", PFShort (Ident "body") (Just (str ""))]) (Just (tref "Props")) Nothing]
             (iden "body"))]

  , astTop "for x of xs do\n  f(x)\n  g(x)"
      [EFor False (pv "x") (iden "xs")
        (EBlock Impl (BlockKey "do") [EApp (iden "f") (args [iden "x"]), EApp (iden "g") (args [iden "x"])])]
  , astTop "const sum = fn(xs) => do\n  let t = 0\n  for x of xs do\n    t += x\n  t"
      [constD (pv "sum")
        (lam [param "xs"]
          (EBlock Impl (BlockKey "do")
            [ letD (pv "t") (num "0")
            , EFor False (pv "x") (iden "xs") (EBlock Impl (BlockKey "do") [EAssign AAdd (iden "t") (iden "x")])
            , iden "t"
            ]))]
  , astTop "while running do\n  step()\n  if done() then break else tick()"
      [EWhile (iden "running")
        (EBlock Impl (BlockKey "do")
          [ EApp (iden "step") []
          , EIfThen (EApp (iden "done") []) EBreak (EApp (iden "tick") [])
          ])]

  , astTop "interface Point where\n  x: number\n  y?: number"
      [EInterface (Ident "Point") [] []
        [ifld "x" (tref "number"), InterfaceField False (key "y") True (tref "number")]]
  , astTop "export interface A extends B where\n  readonly id: string\n  items: Item[]"
      [EExportDecl (EInterface (Ident "A") [] [tref "B"]
        [InterfaceField True (key "id") False (tref "string"), ifld "items" (TEArr (tref "Item"))])]

  , astTop "const f = fn(x) => do\n  if x then return else log(x)\n  x"
      [constD (pv "f")
        (lam [param "x"]
          (EBlock Impl (BlockKey "do")
            [ EIfThen (iden "x") (EReturn Nothing) (EApp (iden "log") (args [iden "x"]))
            , iden "x"
            ]))]
  ]

topFailures :: [Test]
topFailures =
  [ failTop "a\n  b"
  , failTop "do\n  a\n b"
  , failTop "let x =\n1"
  , failTop "f(do\n  a\n  b)"
  , failTop "(a"
  , failTop "a)"
  , failTop "[1, 2"
  , failTop "class A where\n  x = 1\n y"
  , failTop "x\nas T"
  , failTop "import { a }\n  from \"m\"\nx y"
  ]