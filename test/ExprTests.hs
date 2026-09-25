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
    paren (ELit l)          = litDat l
    paren (EIden (Ident s)) = s
    paren (EUnOp op x)      = "(" <> unop2str op <> " " <> paren x <> ")"
    paren (EBinOp op a b)   = "(" <> paren a <> " " <> binop2str op <> " " <> paren b <> ")"
    paren e                 = show e

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

tests :: Test
tests = TestList $ 
  [
    parenTest "a + b * c" "(a + (b * c))"
  , parenTest "a * b + c" "((a * b) + c)"
  , parenTest "a - b - c" "((a - b) - c)"
  , parenTest "a ** b ** c" "(a ** (b ** c))"
  , parenTest "a && b || c" "((a && b) || c)"
  , parenTest "-a * b" "((- a) * b)"
  , parenTest "-a ** b" "((- a) ** b)"
  , parenTest "!a && b" "((! a) && b)"
  , parenTest "(a + b) * c" "((a + b) * c)"
  , parenTest "a | b & c" "(a | (b & c))"
  , parenTest "a & b == c" "(a & (b == c))"
  , parenTest "a << b + c" "(a << (b + c))"
  , parenTest "a || b ?? c" "((a || b) ?? c)"
  , parenTest "typeof -x" "(typeof (- x))"
  , parenTest "a instanceof b" "(a instanceof b)"

  , failTest "a == b == c"
  , failTest "a < b < c"
  , failTest  "a instanceof b instanceof c"

  , astTest "[]"  (EArr [])
  , astTest "{}"  (EObj [])
  , astTest "f()" (EApp (EIden (Ident "f")) [])
  , astTest "x" (EIden (Ident "x"))
  , astTest "42" (ELit (LNum "42"))
  , astTest "fn(x, y) -> x" (ELam [Ident "x", Ident "y"] (EIden (Ident "x")))
  , astTest "if a then b else c" (EIfThen (EIden (Ident "a")) (EIden (Ident "b")) (EIden (Ident "c")))
  , astTest "[1, ...xs, 2]" (EArr [AEPlain  (ELit (LNum "1")), AESpread (EIden (Ident "xs")), AEPlain  (ELit (LNum "2")) ])
  , astTest "{ a: 1, b, ...c, [d]: e }"
      (EObj [
        OFKeyVal   (ObjKey "a") (ELit (LNum "1"))
      , OFShort    (Ident "b")
      , OFSpread   (EIden (Ident "c"))
      , OFComputed (EIden (Ident "d")) (EIden (Ident "e"))
      ])
  , astTest "new Foo(x)" (ENew (EIden (Ident "Foo")) [EIden (Ident "x")])
  , astTest "x!" (ENonNull (EIden (Ident "x")))
  , astTest "a?.b" (EOptMem (EIden (Ident "a")) (MemName "b"))
  , astTest "a.b(c).d[0]"
      (EIndex
         (EMem
            (EApp (EMem (EIden (Ident "a")) (MemName "b")) [EIden (Ident "c")])
            (MemName "d"))
         (ELit (LNum "0")))
  , astTest "{ \"a\": 1 }" (EObj [OFKeyVal (ObjKey "a") (ELit (LNum "1"))])
  , astTest "a.if" (EMem (EIden (Ident "a")) (MemName "if"))
  , astTest "a?.(x)" (EOptApp   (EIden (Ident "a")) [EIden (Ident "x")])
  , astTest "a?.[0]" (EOptIndex (EIden (Ident "a")) (ELit (LNum "0")))
  , astTest "a?.b.c"
      (EMem (EOptMem (EIden (Ident "a")) (MemName "b")) (MemName "c"))
  , astTest "x!.y" (EMem (ENonNull (EIden (Ident "x"))) (MemName "y"))
  , astTest "new Foo()" (ENew (EIden (Ident "Foo")) [])
  , astTest "new a.b.C(x)"
      (ENew (EMem (EMem (EIden (Ident "a")) (MemName "b")) (MemName "C"))
            [EIden (Ident "x")])
  , astTest "fn() -> x" (ELam [] (EIden (Ident "x")))
  , astTest "fn(x) -> fn(y) -> x"
      (ELam [Ident "x"] (ELam [Ident "y"] (EIden (Ident "x"))))
  , astTest "if a then b else if c then d else e"
      (EIfThen (EIden (Ident "a")) (EIden (Ident "b"))
        (EIfThen (EIden (Ident "c")) (EIden (Ident "d")) (EIden (Ident "e"))))

  , failTest "fn -> x"
  , failTest "if a then b"

  , astTest "let x = 1" (EDecl LetDecl (EIden (Ident "x")) (ELit (LNum "1")))
  , astTest "const x = 1" (EDecl ConstDecl (EIden (Ident "x")) (ELit (LNum "1")))
  , astTest "let x = a + b" (EDecl LetDecl (EIden (Ident "x")) (EBinOp Add (EIden (Ident "a")) (EIden (Ident "b"))))

  , astTest "let [a, b] = xs" (EDecl LetDecl (EArr [AEPlain (EIden (Ident "a")), AEPlain (EIden (Ident "b"))]) (EIden (Ident "xs")))
  , astTest "let {a, b} = o" (EDecl LetDecl (EObj [OFShort (Ident "a"), OFShort (Ident "b")]) (EIden (Ident "o")))

  , astTest "let x.y = 1" (EDecl LetDecl (EMem (EIden (Ident "x")) (MemName "y")) (ELit (LNum "1")))

  , failTest "let x"
  , failTest "let = 1"

  , astTest "do { a; b }" (EBlock Expl (BlockKey "do") [EIden (Ident "a"), EIden (Ident "b")])
  , astTest "do {}" (EBlock Expl (BlockKey "do") [])
  , astTest "where { a }" (EBlock Expl (BlockKey "where") [EIden (Ident "a")])
  , astTest "do { a; }" (EBlock Expl (BlockKey "do") [EIden (Ident "a")])

  , failTest "do { a b }"

  , astTest "let x = do { a; b }" (EDecl LetDecl (EIden (Ident "x")) (EBlock Expl (BlockKey "do") [EIden (Ident "a"), EIden (Ident "b")]))
  ]
  <>
  [
    astTop "a\nb\nc" [EIden (Ident "a"), EIden (Ident "b"), EIden (Ident "c")]
  , astTop "let x = 1" [EDecl LetDecl (EIden (Ident "x")) (ELit (LNum "1"))]
  , astTop "do\n  a\n  b" [EBlock Impl (BlockKey "do") [EIden (Ident "a"), EIden (Ident "b")]]
  , astTop "do\n  a\n  do\n    b" [EBlock Impl (BlockKey "do") [ EIden (Ident "a") , EBlock Impl (BlockKey "do") [EIden (Ident "b")] ]]
  , astTop "x\ndo\ny" [ EIden (Ident "x") , EBlock Impl (BlockKey "do") [] , EIden (Ident "y") ]
  , astTop "if a then b else c" [EIfThen (EIden (Ident "a")) (EIden (Ident "b")) (EIden (Ident "c"))]
  ]