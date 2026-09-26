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

num :: String -> Expr
num = ELit . LNum

tref :: String -> TExpr
tref n = TEApp [Ident n] []

args :: [Expr] -> [Elem]
args = fmap ElemPlain

param :: String -> LamParam
param x = LPPlain (Ident x) Nothing Nothing

paramT :: String -> TExpr -> LamParam
paramT x t = LPPlain (Ident x) (Just t) Nothing

paramD :: String -> Expr -> LamParam
paramD x d = LPPlain (Ident x) Nothing (Just d)

rest :: String -> LamParam
rest x = LPRest (Ident x) Nothing

lam :: [LamParam] -> Expr -> Expr
lam ps = ELam (FuncSig [] ps Nothing)

letD, constD :: Expr -> Expr -> Expr
letD   p = EDecl LetDecl   p Nothing
constD p = EDecl ConstDecl p Nothing

key :: String -> PropKey
key = PKMem . MNPub

sig0 :: FuncSig
sig0 = FuncSig [] [] Nothing

cdecl :: Bool -> Maybe String -> [String] -> Maybe TExpr -> [TExpr] -> [ClassMem] -> Expr
cdecl a n ps e is ms = EClass (ClassDecl a (Ident <$> n) (Ident <$> ps) e is ms)

cls :: String -> [ClassMem] -> Expr
cls n = cdecl False (Just n) [] Nothing []

fieldI :: String -> Expr -> ClassMem
fieldI k e = CMField [] (key k) False Nothing (Just e)

meth :: String -> [LamParam] -> Expr -> ClassMem
meth k ps body = CMethod [] (key k) (FuncSig [] ps Nothing) (Just body)

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
  , lambdas
  , params
  , signatures
  , trailingCommas
  , conditionals
  , assignment
  , declarations
  , annotatedDecls
  , keywordBindings
  , typeDecls
  , classes
  , reservedMembers
  , blocks
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
  , astTest "\"a\"" (ELit (LStr "a"))
  , astTest "true" (ELit (LBool "true"))
  , astTest "null" (ELit (LNull "null"))
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

  -- only plain identifiers can be shorthand
  , failTest "{ \"a\" }"
  , failTest "{ 1 }"
  , failTest "{ [k] }"
  , failTest "{ \"a\" = 1 }"
  , failTest "{ [k]: }"

  -- private names are class-only
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
  ]

unary :: [Test]
unary =
  [ astTest (unop2str op <> " x") (EUnOp op (iden "x")) | op <- [minBound ..] ]
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
  , astTest "fn(...xs: number[]) => xs" (lam [LPRest (Ident "xs") (Just (TEArr (tref "number")))] (iden "xs"))
  , astTest "fn(a: A, ...xs: A[]) => a" (lam [paramT "a" (tref "A"), LPRest (Ident "xs") (Just (TEArr (tref "A")))] (iden "a"))

  , astTest "fn(x = 1) => x" (lam [paramD "x" (num "1")] (iden "x"))
  , astTest "fn(x, y = 2) => y" (lam [param "x", paramD "y" (num "2")] (iden "y"))
  , astTest "fn(x: number = 1) => x" (lam [LPPlain (Ident "x") (Just (tref "number")) (Just (num "1"))] (iden "x"))
  , astTest "fn(x = a + b) => x" (lam [paramD "x" (EBinOp Add (iden "a") (iden "b"))] (iden "x"))
  , astTest "fn(f = fn() => 1) => f" (lam [paramD "f" (lam [] (num "1"))] (iden "f"))
  , astTest "fn(x = 1, ...ys) => x" (lam [paramD "x" (num "1"), rest "ys"] (iden "x"))

  , failTest "fn(...xs, a) => a"
  , failTest "fn(...xs,) => xs"
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
      (ELam (FuncSig [] [param "x"] (Just (tref "number"))) (iden "x"))
  , astTest "fn(): A | B => x"
      (ELam (FuncSig [] [] (Just (TEBinOp TUnion (tref "A") (tref "B")))) (iden "x"))
  , astTest "fn(): A[] => x"
      (ELam (FuncSig [] [] (Just (TEArr (tref "A")))) (iden "x"))
  , astTest "fn(...xs: A[]): number => n"
      (ELam (FuncSig [] [LPRest (Ident "xs") (Just (TEArr (tref "A")))] (Just (tref "number"))) (iden "n"))
  , astTest "forall T. fn(x: T) => x"
      (ELam (FuncSig [Ident "T"] [paramT "x" (tref "T")] Nothing) (iden "x"))
  , astTest "forall T. fn(x: T): T => x"
      (ELam (FuncSig [Ident "T"] [paramT "x" (tref "T")] (Just (tref "T"))) (iden "x"))
  , astTest "forall K V. fn(m: Map(K, V)): V[] => m"
      (ELam (FuncSig [Ident "K", Ident "V"]
                     [paramT "m" (TEApp [Ident "Map"] [tref "K", tref "V"])]
                     (Just (TEArr (tref "V"))))
            (iden "m"))

  , astTest "fn(): (fn() => A) => f"
      (ELam (FuncSig [] [] (Just (TEFunc [] [] (tref "A")))) (iden "f"))
  , astTest "fn(): fn() => A => f"
      (ELam (FuncSig [] [] (Just (TEFunc [] [] (tref "A")))) (iden "f"))

  , astTest "f(forall T. fn(x: T) => x)"
      (EApp (iden "f") (args [ELam (FuncSig [Ident "T"] [paramT "x" (tref "T")] Nothing) (iden "x")]))
  , astTest "{ id: forall T. fn(x: T): T => x }"
      (EObj [OFKeyVal (key "id")
               (ELam (FuncSig [Ident "T"] [paramT "x" (tref "T")] (Just (tref "T"))) (iden "x"))])

  , failTest "forall T. x"
  , failTest "forall. fn() => x"
  , failTest "forall T fn() => x"
  , failTest "forall 1. fn() => x"
  , failTest "forall T. forall U. fn() => x"
  , failTest "fn(x): => x"
  , failTest "fn(x): A"
  , failTest "fn(x) : A x"
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

  , failTest "x ="
  , failTest "= 1"
  , failTest "x += "
  , failTest "x = = 1"
  ]

declarations :: [Test]
declarations =
  [ astTest "let x = 1" (letD (iden "x") (num "1"))
  , astTest "const x = 1" (constD (iden "x") (num "1"))
  , astTest "let x = a + b" (letD (iden "x") (EBinOp Add (iden "a") (iden "b")))
  , astTest "const f = fn(x) => x" (constD (iden "f") (lam [param "x"] (iden "x")))
  , astTest "let x = if a then b else c" (letD (iden "x") (EIfThen (iden "a") (iden "b") (iden "c")))
  , astTest "let x = do { a; b }" (letD (iden "x") (EBlock Expl (BlockKey "do") [iden "a", iden "b"]))

  , astTest "const x = y = 1" (constD (iden "x") (EAssign AEq (iden "y") (num "1")))
  , astTest "let x = y += 1" (letD (iden "x") (EAssign AAdd (iden "y") (num "1")))

  , astTest "let [a, b] = xs" (letD (EArr [ElemPlain (iden "a"), ElemPlain (iden "b")]) (iden "xs"))
  , astTest "let [a, ...rest] = xs" (letD (EArr [ElemPlain (iden "a"), ElemSpread (iden "rest")]) (iden "xs"))
  , astTest "let [a, [b, c]] = xs"
      (letD (EArr [ElemPlain (iden "a"), ElemPlain (EArr [ElemPlain (iden "b"), ElemPlain (iden "c")])]) (iden "xs"))
  , astTest "let [a = 1] = xs" (letD (EArr [ElemPlain (EAssign AEq (iden "a") (num "1"))]) (iden "xs"))
  , astTest "let {a, b} = o" (letD (EObj [OFShort (Ident "a"), OFShort (Ident "b")]) (iden "o"))
  , astTest "let {a: x, ...o2} = o" (letD (EObj [OFKeyVal (key "a") (iden "x"), OFSpread (iden "o2")]) (iden "o"))
  , astTest "let {a: {b}} = o" (letD (EObj [OFKeyVal (key "a") (EObj [OFShort (Ident "b")])]) (iden "o"))
  , astTest "let {a = 1} = o" (letD (EObj [OFShortInit (Ident "a") (num "1")]) (iden "o"))

  , astTest "let x.y = 1" (letD (EMem (iden "x") (MNPub "y")) (num "1"))

  , failTest "let x"
  , failTest "const x"
  , failTest "let = 1"
  , failTest "let x = "
  , failTest "let x += 1"
  ]

annotatedDecls :: [Test]
annotatedDecls =
  [ astTest "let x: number = 1"
      (EDecl LetDecl (iden "x") (Just (tref "number")) (num "1"))
  , astTest "const x: A | null = y"
      (EDecl ConstDecl (iden "x") (Just (TEBinOp TUnion (tref "A") (TELit (LNull "null")))) (iden "y"))
  , astTest "const f: fn(x: A) => B = g"
      (EDecl ConstDecl (iden "f")
        (Just (TEFunc [] [TFPIdent (Ident "x") False (tref "A")] (tref "B"))) (iden "g"))
  , astTest "const id: forall T. fn(x: T) => T = fn(x) => x"
      (EDecl ConstDecl (iden "id")
        (Just (TEFunc [Ident "T"] [TFPIdent (Ident "x") False (tref "T")] (tref "T")))
        (lam [param "x"] (iden "x")))
  , astTest "let [a, b]: [A, B] = xs"
      (EDecl LetDecl (EArr [ElemPlain (iden "a"), ElemPlain (iden "b")])
        (Just (TETuple [TTEPlain (tref "A"), TTEPlain (tref "B")])) (iden "xs"))
  , astTest "let {a}: O = o"
      (EDecl LetDecl (EObj [OFShort (Ident "a")]) (Just (tref "O")) (iden "o"))
  , astTest "const first = forall T. fn(xs: T[]): T | undefined => xs[0]"
      (constD (iden "first")
        (ELam (FuncSig [Ident "T"] [paramT "xs" (TEArr (tref "T"))]
                       (Just (TEBinOp TUnion (tref "T") (tref "undefined"))))
              (EIndex (iden "xs") (num "0"))))

  , failTest "let x: = 1"
  , failTest "let x: A"
  , failTest "let x: A 1"
  , failTest "let x:: A = 1"
  ]

-- strict-mode reserved words are keywords; TS-only contextual words stay identifiers
keywordBindings :: [Test]
keywordBindings =
  [ failTest "let static = 1"
  , failTest "let protected = 1"
  , failTest "let extends = 1"
  , failTest "let implements = 1"
  , failTest "let class = 1"
  , failTest "fn(static) => 1"

  , astTest "let readonly = 1" (letD (iden "readonly") (num "1"))
  , astTest "let abstract = 1" (letD (iden "abstract") (num "1"))
  , astTest "let override = 1" (letD (iden "override") (num "1"))
  , astTest "abstract + 1" (EBinOp Add (iden "abstract") (num "1"))

  -- keywords remain valid as property names
  , astTest "express.static(dir)" (EApp (EMem (iden "express") (MNPub "static")) (args [iden "dir"]))
  , astTest "a.extends" (EMem (iden "a") (MNPub "extends"))
  , astTest "{ static: 1, class: 2 }" (EObj [OFKeyVal (key "static") (num "1"), OFKeyVal (key "class") (num "2")])
  , astTest "{ readonly: true }" (EObj [OFKeyVal (key "readonly") (ELit (LBool "true"))])
  ]

typeDecls :: [Test]
typeDecls =
  [ astTest "type Id = number" (ETypeDecl (Ident "Id") [] (tref "number"))
  , astTest "type U = A | B" (ETypeDecl (Ident "U") [] (TEBinOp TUnion (tref "A") (tref "B")))
  , astTest "type Pair(A, B) = [A, B]"
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
  ]

classes :: [Test]
classes =
  -- fields
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
  , astTest "class A where { type = \"a\" }" (cls "A" [fieldI "type" (ELit (LStr "a"))])

  -- methods
  , astTest "class A where { fn f() => 1 }" (cls "A" [meth "f" [] (num "1")])
  , astTest "class A where { fn f(x: A): B => x }"
      (cls "A" [CMethod [] (key "f") (FuncSig [] [paramT "x" (tref "A")] (Just (tref "B"))) (Just (iden "x"))])
  , astTest "class A where { fn constructor(x) => this.x = x }"
      (cls "A" [meth "constructor" [param "x"] (EAssign AEq (EMem (iden "this") (MNPub "x")) (iden "x"))])
  , astTest "class A where { forall T. fn id(x: T): T => x }"
      (cls "A" [CMethod [] (key "id") (FuncSig [Ident "T"] [paramT "x" (tref "T")] (Just (tref "T"))) (Just (iden "x"))])
  , astTest "class A where { fn #helper() => 1 }" (cls "A" [CMethod [] (PKMem (MNPriv "helper")) sig0 (Just (num "1"))])
  , astTest "class A where { fn [k]() => 1 }" (cls "A" [CMethod [] (PKComp (iden "k")) sig0 (Just (num "1"))])
  , astTest "class A where { fn f(...xs) => xs }" (cls "A" [meth "f" [rest "xs"] (iden "xs")])
  , astTest "class A where { fn delete(id) => id }" (cls "A" [meth "delete" [param "id"] (iden "id")])

  -- modifiers
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
        [CMethod [MMAbstract] (key "f") (FuncSig [] [] (Just (tref "number"))) Nothing])

  -- header
  , astTest "class B extends A where {}" (cdecl False (Just "B") [] (Just (tref "A")) [] [])
  , astTest "class Box(T) extends Base(T) implements I, J(T) where {}"
      (cdecl False (Just "Box") ["T"]
        (Just (TEApp [Ident "Base"] [tref "T"]))
        [tref "I", TEApp [Ident "J"] [tref "T"]]
        [])
  , astTest "class where {}" (cdecl False Nothing [] Nothing [] [])
  , astTest "class extends A where {}" (cdecl False Nothing [] (Just (tref "A")) [] [])
  , astTest "const A = class where { x = 1 }"
      (constD (iden "A") (cdecl False Nothing [] Nothing [] [fieldI "x" (num "1")]))

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

-- member-starting words can't be bare member names; a string key is the escape hatch
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

blocks :: [Test]
blocks =
  [ astTest "do {}" (EBlock Expl (BlockKey "do") [])
  , astTest "do { a }" (EBlock Expl (BlockKey "do") [iden "a"])
  , astTest "do { a; b }" (EBlock Expl (BlockKey "do") [iden "a", iden "b"])
  , astTest "do { a; b; c }" (EBlock Expl (BlockKey "do") [iden "a", iden "b", iden "c"])
  , astTest "do { a; }" (EBlock Expl (BlockKey "do") [iden "a"])
  , astTest "do { let x = 1; x }" (EBlock Expl (BlockKey "do") [letD (iden "x") (num "1"), iden "x"])
  , astTest "do { x = 1 }" (EBlock Expl (BlockKey "do") [EAssign AEq (iden "x") (num "1")])
  , astTest "do { do { a } }" (EBlock Expl (BlockKey "do") [EBlock Expl (BlockKey "do") [iden "a"]])
  , astTest "where { a }" (EBlock Expl (BlockKey "where") [iden "a"])
  , astTest "where { a; b }" (EBlock Expl (BlockKey "where") [iden "a", iden "b"])

  , failTest "do { a b }"
  , failTest "do a"
  , failTest "do { a"
  , failTest "do }"
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
  , astTop "let x = 1" [letD (iden "x") (num "1")]
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

  , astTop "let x =\n  1" [letD (iden "x") (num "1")]
  , astTop "a\n  + b" [EBinOp Add (iden "a") (iden "b")]
  , astTop "if a\n  then b\n  else c" [EIfThen (iden "a") (iden "b") (iden "c")]

  , astTop "let xs = [\n  1,\n  2\n]" [letD (iden "xs") (EArr [ElemPlain (num "1"), ElemPlain (num "2")])]
  , astTop "f(\n  a,\n  b\n)" [EApp (iden "f") (args [iden "a", iden "b"])]
  , astTop "{\n  a: 1,\n  b: 2\n}" [EObj [OFKeyVal (key "a") (num "1"), OFKeyVal (key "b") (num "2")]]

  , astTop "let xs = [\n  1,\n  2,\n]" [letD (iden "xs") (EArr [ElemPlain (num "1"), ElemPlain (num "2")])]
  , astTop "f(\n  a,\n  b,\n)" [EApp (iden "f") (args [iden "a", iden "b"])]
  , astTop "{\n  a: 1,\n  b: 2,\n}" [EObj [OFKeyVal (key "a") (num "1"), OFKeyVal (key "b") (num "2")]]
  , astTop "const f = fn(\n  a,\n  ...rest\n) => a" [constD (iden "f") (lam [param "a", rest "rest"] (iden "a"))]

  , astTop "const first = forall T.\n  fn(xs: T[]): T | undefined => xs[0]"
      [constD (iden "first")
        (ELam (FuncSig [Ident "T"] [paramT "xs" (TEArr (tref "T"))]
                       (Just (TEBinOp TUnion (tref "T") (tref "undefined"))))
              (EIndex (iden "xs") (num "0")))]

  , astTop "type Color = \"red\"\n  | \"green\""
      [ETypeDecl (Ident "Color") [] (TEBinOp TUnion (TELit (LStr "red")) (TELit (LStr "green")))]
  , astTop "type A = number\nlet x = 1" [ETypeDecl (Ident "A") [] (tref "number"), letD (iden "x") (num "1")]

  , astTop "type Point = { x: number, y: number }\nlet p = { x: 1, y: 2 }\nlet dist = fn(a: Point) => do\n  let dx = a.x\n  dx * dx"
      [ ETypeDecl (Ident "Point") []
          (TEObj [TOFKeyVal (key "x") False (tref "number"), TOFKeyVal (key "y") False (tref "number")])
      , letD (iden "p") (EObj [OFKeyVal (key "x") (num "1"), OFKeyVal (key "y") (num "2")])
      , letD (iden "dist")
          (lam [paramT "a" (tref "Point")]
            (EBlock Impl (BlockKey "do")
              [ letD (iden "dx") (EMem (iden "a") (MNPub "x"))
              , EBinOp Mul (iden "dx") (iden "dx") ]))
      ]

  -- classes with layout
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
  ]