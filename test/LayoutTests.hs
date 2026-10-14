module LayoutTests (tests) where

import Test.HUnit

import Lexer (TokInfo(tokVal), Tok(..), Lit(..), runLex, sm, oa, ca, ob, cb, op, cp, oq, cq, dk, wk)
import Layout

ok :: String -> [Tok] -> Test
ok s expected = TestCase $
  case runLex s of
    Left e   -> assertFailure ("lex failed on " <> show s <> ": " <> show e)
    Right ts -> case layout ts of
      Left e    -> assertFailure ("layout failed on " <> show s <> ": " <> show e)
      Right ts' -> fmap tokVal ts' @?= expected

layFail :: String -> Test
layFail s = TestCase $
  case runLex s of
    Left e   -> assertFailure ("lex failed on " <> show s <> ": " <> show e)
    Right ts -> case layout ts of
      Left _    -> return ()
      Right ts' -> assertFailure ("expected layout failure on " <> show s <> ", got " <> show (fmap tokVal ts'))

a, b, c, x, y, f, g :: Tok
a = TIden "a"
b = TIden "b"
c = TIden "c"
x = TIden "x"
y = TIden "y"
f = TIden "f"
g = TIden "g"

tests :: Test
tests = TestList $
  concat [basics, continuation, implicitBlocks, explicitBraces, brackets, mixed, multilineTokens, fails]

basics :: [Test]
basics =
  [ ok "" [oa, ca, TEOF]
  , ok "x" [oa, x, ca, TEOF]
  , ok "  x" [oa, x, ca, TEOF]
  , ok "a b" [oa, a, b, ca, TEOF]
  , ok "a\nb\nc" [oa, a, sm, b, sm, c, ca, TEOF]
  , ok "  a\n  b" [oa, a, sm, b, ca, TEOF]
  , ok "a\n\nb" [oa, a, sm, b, ca, TEOF]
  , ok "a // c\nb" [oa, a, sm, b, ca, TEOF]
  , ok "a\n// c\nb" [oa, a, sm, b, ca, TEOF]
  , ok "a;b" [oa, a, sm, b, ca, TEOF]
  ]

continuation :: [Test]
continuation =
  [ ok "a\n  b" [oa, a, b, ca, TEOF]
  , ok "a\n  b\nc" [oa, a, b, sm, c, ca, TEOF]
  , ok "A\n  | B" [oa, TIden "A", TSym "|", TIden "B", ca, TEOF]
  , ok "A\n  | B\n  | C" [oa, TIden "A", TSym "|", TIden "B", TSym "|", TIden "C", ca, TEOF]
  , ok "A\n| B" [oa, TIden "A", sm, TSym "|", TIden "B", ca, TEOF]
  , ok "if a\nthen b\nelse c" [oa, TKey "if", a, TKey "then", b, TKey "else", c, ca, TEOF]
  , ok "try\n  a\ncatch => b" [oa, TKey "try", a, TKey "catch", TSym "=>", b, ca, TEOF]
  , ok  "try\n  a\nfinally b\nc" [oa, TKey "try", a, TKey "finally", b, sm, c, ca, TEOF]

  , ok  "let t = A\n  | B\nlet u = C"
      [ oa, TKey "let", TIden "t", TSym "=", TIden "A", TSym "|", TIden "B", sm , TKey "let", TIden "u", TSym "=", TIden "C", ca, TEOF ]
  ]

implicitBlocks :: [Test]
implicitBlocks =
  [ ok "do\n  a\n  b" [oa, dk, oa, a, sm, b, ca, ca, TEOF]
  , ok "where\n  a" [oa, wk, oa, a, ca, ca, TEOF]
  , ok "where\n  a\n  b" [oa, wk, oa, a, sm, b, ca, ca, TEOF]
  , ok "do a" [oa, dk, oa, a, ca, ca, TEOF]
  , ok "do a\n   b" [oa, dk, oa, a, sm, b, ca, ca, TEOF]
  , ok "do a\n  b" [oa, dk, oa, a, ca, b, ca, TEOF]
  , ok "x where a" [oa, x, wk, oa, a, ca, ca, TEOF]
  , ok "do" [oa, dk, oa, ca, ca, TEOF]
  , ok "x\ndo\ny" [oa, x, sm, dk, oa, ca, sm, y, ca, TEOF]
  , ok "do\n  a\n  b\nc" [oa, dk, oa, a, sm, b, ca, sm, c, ca, TEOF]
  , ok "do\n  a\n b" [oa, dk, oa, a, ca, b, ca, TEOF] -- dedent between levels
  , ok "do\n  do\n    a" [oa, dk, oa, dk, oa, a, ca, ca, ca, TEOF]
  , ok "do\n  do\n    a\nb" [oa, dk, oa, dk, oa, a, ca, ca, sm, b, ca, TEOF]
  , ok "do\n  a\n  do\n    b" [oa, dk, oa, a, sm, dk, oa, b, ca, ca, ca, TEOF]
  , ok "do\n  a\n  do\n    b\n  c" [oa, dk, oa, a, sm, dk, oa, b, ca, sm, c, ca, ca, TEOF]
  , ok "do\n  a\n  where\n    b" [oa, dk, oa, a, sm, wk, oa, b, ca, ca, ca, TEOF]
  , ok "f = do\n  a" [oa, f, TSym "=", dk, oa, a, ca, ca, TEOF]
  , ok  "let x = do\n    a\n    b\ny" [oa, TKey "let", x, TSym "=", dk, oa, a, sm, b, ca, sm, y, ca, TEOF] 
  , ok "do\n  a\n    + b\n  c" [oa, dk, oa, a, TSym "+", b, sm, c, ca, ca, TEOF]
  , ok "do\n  a\n\n  b" [oa, dk, oa, a, sm, b, ca, ca, TEOF]
  , ok "do\n  a\n  // c\n  b" [oa, dk, oa, a, sm, b, ca, ca, TEOF]
  , ok "do\n\ta\n\tb" [oa, dk, oa, a, sm, b, ca, ca, TEOF]
  , ok "do\n  a;b" [oa, dk, oa, a, sm, b, ca, ca, TEOF]
  ]

explicitBraces :: [Test]
explicitBraces =
  [ ok "do {}" [oa, dk, ob, cb, ca, TEOF]
  , ok "do { a; b }" [oa, dk, ob, a, sm, b, cb, ca, TEOF]
  , ok "where { a }" [oa, wk, ob, a, cb, ca, TEOF]
  , ok "do { a\n b }" [oa, dk, ob, a, b, cb, ca, TEOF]
  , ok "do {\n  a\n}" [oa, dk, ob, a, cb, ca, TEOF]
  , ok "do {\na\nb\n}" [oa, dk, ob, a, b, cb, ca, TEOF]
  , ok "do\n  { a }" [oa, dk, ob, a, cb, ca, TEOF]
  , ok "{ do\n    a\n    b\n}" [oa, ob, dk, oa, a, sm, b, ca, cb, ca, TEOF]
  ]

brackets :: [Test]
brackets =
  [ ok "(a)" [oa, op, a, cp, ca, TEOF]
  , ok "([{}])" [oa, op, oq, ob, cb, cq, cp, ca, TEOF]
  , ok "f(a)\ng(b)" [oa, f, op, a, cp, sm, g, op, b, cp, ca, TEOF]
  , ok "[\n  x,\n  y\n]" [oa, oq, x, TSym ",", y, cq, ca, TEOF]
  , ok "f(\n  a\n)" [oa, f, op, a, cp, ca, TEOF]
  , ok "{\n  a: A\n}" [oa, ob, a, TSym ":", TIden "A", cb, ca, TEOF]
  , ok "f(\na,\nb\n)" [oa, f, op, a, TSym ",", b, cp, ca, TEOF]
  , ok "x = [\na\n]" [oa, x, TSym "=", oq, a, cq, ca, TEOF]
  , ok "f(\n  a\n)\ng" [oa, f, op, a, cp, sm, g, ca, TEOF]
  ]

mixed :: [Test]
mixed =
  [ ok "f(do\n  a\n)" [oa, f, op, dk, oa, a, ca, cp, ca, TEOF]
  , ok "[do\n  a\n]" [oa, oq, dk, oa, a, ca, cq, ca, TEOF]
  ]

multilineTokens :: [Test]
multilineTokens =
  [ ok "x = \"a\nb\".trim()" [oa, x, TSym "=", ml, TSym ".", TIden "trim", op, cp, ca, TEOF]
  , ok "x = \"a\nb\"\ny" [oa, x, TSym "=", ml, sm, y, ca, TEOF]
  , ok "do\n  x = \"a\nb\"\n  y" [oa, dk, oa, x, TSym "=", ml, sm, y, ca, ca, TEOF]
  ]
  where
  ml = TLit (LStr "a\nb")

fails :: [Test]
fails = fmap layFail
  [ ")", "]", "}", "a)", "(a))"
  , "(]", "[)", "{)", "(}"
  , "f(a", "[a", "{a", "((a)", "do { a"
  , "f(do\n  a\n  b)"
  , "f(do a)"
  , "{ do\n    a\n    b }"
  , "do\n  a)"
  ]