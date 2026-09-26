module LayoutTests (tests) where

import Test.HUnit

import Lexer (Tok(TEOF, TKey, TIden), TokInfo(tokVal), runLex, sm, oa, ca, ob, cb, op, cp, oq, cq, dk, wk)
import Layout
import Text.Megaparsec.Pos (SourcePos(..), unPos)
import Lexer (Tok(..))

layTest :: String -> [Tok] -> Test
layTest s expected = TestCase $
  case runLex s of
    Left e   -> assertFailure ("lex failed on " <> show s <> ": " <> show e)
    Right ts -> case layout ts of
      Left e    -> assertFailure ("layout failed on " <> show s <> ": " <> show e)
      Right ts' -> (fmap tokVal ts') @?= expected

layFail :: String -> Test
layFail s = TestCase $
  case runLex s of
    Left e   -> assertFailure ("lex failed on " <> show s <> ": " <> show e)
    Right ts -> case layout ts of
      Left _    -> return ()
      Right ts' -> assertFailure ("expected layout failure on " <> show s <> ", got " <> show (fmap tokVal ts'))

tests :: Test
tests = TestList $ concat
  [ basics
  , continuation
  , implicitBlocks
  , explicitBraces
  , brackets
  , mixed
  , fails
  ]

basics :: [Test]
basics =
  [ layTest "" [oa, ca, TEOF]
  , layTest "x" [oa, TIden "x", ca, TEOF]
  , layTest "  x" [oa, TIden "x", ca, TEOF]
  , layTest "a b" [oa, TIden "a", TIden "b", ca, TEOF]

  , layTest "a\nb\nc" [oa, TIden "a", sm, TIden "b", sm, TIden "c", ca, TEOF]
  , layTest "  a\n  b" [oa, TIden "a", sm, TIden "b", ca, TEOF]

  , layTest "a\n\nb" [oa, TIden "a", sm, TIden "b", ca, TEOF]
  , layTest "a // c\nb" [oa, TIden "a", sm, TIden "b", ca, TEOF]
  , layTest "a\n// c\nb" [oa, TIden "a", sm, TIden "b", ca, TEOF]

  , layTest "a;b" [oa, TIden "a", sm, TIden "b", ca, TEOF]
  ]

continuation :: [Test]
continuation =
  [ layTest "a\n  b" [oa, TIden "a", TIden "b", ca, TEOF]
  , layTest "a\n  b\nc" [oa, TIden "a", TIden "b", sm, TIden "c", ca, TEOF]
  , layTest "A\n  | B" [oa, TIden "A", TSym "|", TIden "B", ca, TEOF]
  , layTest "A\n  | B\n  | C" [oa, TIden "A", TSym "|", TIden "B", TSym "|", TIden "C", ca, TEOF]

  , layTest "A\n| B" [oa, TIden "A", sm, TSym "|", TIden "B", ca, TEOF]

  , layTest "type T = A\n  | B\ntype U = C"
      [oa, TKey "type", TIden "T", TSym "=", TIden "A", TSym "|", TIden "B", sm,
       TKey "type", TIden "U", TSym "=", TIden "C", ca, TEOF]
  ]

implicitBlocks :: [Test]
implicitBlocks =
  [ layTest "do\n  a\n  b" [oa, dk, oa, TIden "a", sm, TIden "b", ca, ca, TEOF]
  , layTest "where\n  a" [oa, wk, oa, TIden "a", ca, ca, TEOF]
  , layTest "where\n  a\n  b" [oa, wk, oa, TIden "a", sm, TIden "b", ca, ca, TEOF]

  , layTest "do a" [oa, dk, oa, TIden "a", ca, ca, TEOF]
  , layTest "do a\n   b" [oa, dk, oa, TIden "a", sm, TIden "b", ca, ca, TEOF]
  , layTest "do a\n  b" [oa, dk, oa, TIden "a", ca, TIden "b", ca, TEOF]
  , layTest "x where a" [oa, TIden "x", wk, oa, TIden "a", ca, ca, TEOF]

  , layTest "do" [oa, dk, oa, ca, ca, TEOF]
  , layTest "x\ndo\ny" [oa, TIden "x", sm, dk, oa, ca, sm, TIden "y", ca, TEOF]

  , layTest "do\n  a\n  b\nc" [oa, dk, oa, TIden "a", sm, TIden "b", ca, sm, TIden "c", ca, TEOF]
  , layTest "do\n  do\n    a\nb" [oa, dk, oa, dk, oa, TIden "a", ca, ca, sm, TIden "b", ca, TEOF]

  , layTest "do\n  a\n  do\n    b" [oa, dk, oa, TIden "a", sm, dk, oa, TIden "b", ca, ca, ca, TEOF]
  , layTest "do\n  a\n  do\n    b\n  c" [oa, dk, oa, TIden "a", sm, dk, oa, TIden "b", ca, sm, TIden "c", ca, ca, TEOF]
  , layTest "do\n  a\n  where\n    b" [oa, dk, oa, TIden "a", sm, wk, oa, TIden "b", ca, ca, ca, TEOF]

  , layTest "f = do\n  a" [oa, TIden "f", TSym "=", dk, oa, TIden "a", ca, ca, TEOF]
  , layTest "let x = do\n    a\n    b\ny"
      [oa, TKey "let", TIden "x", TSym "=", dk, oa, TIden "a", sm, TIden "b", ca, sm, TIden "y", ca, TEOF]

  , layTest "do\n  a\n    + b\n  c" [oa, dk, oa, TIden "a", TSym "+", TIden "b", sm, TIden "c", ca, ca, TEOF]
  , layTest "do\n  a\n\n  b" [oa, dk, oa, TIden "a", sm, TIden "b", ca, ca, TEOF]
  , layTest "do\n  a\n  // c\n  b" [oa, dk, oa, TIden "a", sm, TIden "b", ca, ca, TEOF]
  , layTest "do\n\ta\n\tb" [oa, dk, oa, TIden "a", sm, TIden "b", ca, ca, TEOF]
  , layTest "do\n  a;b" [oa, dk, oa, TIden "a", sm, TIden "b", ca, ca, TEOF]
  ]

explicitBraces :: [Test]
explicitBraces =
  [ layTest "do {}" [oa, dk, ob, cb, ca, TEOF]
  , layTest "do { a; b }" [oa, dk, ob, TIden "a", sm, TIden "b", cb, ca, TEOF]
  , layTest "where { a }" [oa, wk, ob, TIden "a", cb, ca, TEOF]

  , layTest "do { a\n b }" [oa, dk, ob, TIden "a", TIden "b", cb, ca, TEOF]
  , layTest "do {\n  a\n}" [oa, dk, ob, TIden "a", cb, ca, TEOF]
  , layTest "do {\na\nb\n}" [oa, dk, ob, TIden "a", TIden "b", cb, ca, TEOF]

  , layTest "do\n  { a }" [oa, dk, ob, TIden "a", cb, ca, TEOF]

  , layTest "{ do\n    a\n    b\n}" [oa, ob, dk, oa, TIden "a", sm, TIden "b", ca, cb, ca, TEOF]
  ]

brackets :: [Test]
brackets =
  [ layTest "(a)" [oa, op, TIden "a", cp, ca, TEOF]
  , layTest "([{}])" [oa, op, oq, ob, cb, cq, cp, ca, TEOF]
  , layTest "f(a)\ng(b)" [oa, TIden "f", op, TIden "a", cp, sm, TIden "g", op, TIden "b", cp, ca, TEOF]

  , layTest "[\n  x,\n  y\n]" [oa, oq, TIden "x", TSym ",", TIden "y", cq, ca, TEOF]
  , layTest "f(\n  a\n)" [oa, TIden "f", op, TIden "a", cp, ca, TEOF]
  , layTest "{\n  a: A\n}" [oa, ob, TIden "a", TSym ":", TIden "A", cb, ca, TEOF]

  , layTest "f(\na,\nb\n)" [oa, TIden "f", op, TIden "a", TSym ",", TIden "b", cp, ca, TEOF]
  , layTest "x = [\na\n]" [oa, TIden "x", TSym "=", oq, TIden "a", cq, ca, TEOF]

  , layTest "f(\n  a\n)\ng" [oa, TIden "f", op, TIden "a", cp, sm, TIden "g", ca, TEOF]
  ]

mixed :: [Test]
mixed =
  [ layTest "f(do\n  a\n)" [oa, TIden "f", op, dk, oa, TIden "a", ca, cp, ca, TEOF]
  , layTest "[do\n  a\n]" [oa, oq, dk, oa, TIden "a", ca, cq, ca, TEOF]
  ]

fails :: [Test]
fails =
  [ layFail ")"
  , layFail "]"
  , layFail "}"
  , layFail "a)"
  , layFail "(a))"

  , layFail "(]"
  , layFail "[)"
  , layFail "{)"
  , layFail "(}"

  , layFail "f(a"
  , layFail "[a"
  , layFail "{a"
  , layFail "((a)"
  , layFail "do { a"

  , layFail "f(do\n  a\n  b)"
  , layFail "f(do a)"
  , layFail "{ do\n    a\n    b }"
  , layFail "do\n  a)"
  ]