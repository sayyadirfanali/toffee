module LayoutTests (tests) where

import Test.HUnit

import Lexer (Tok(TEOF, TKey, TIden), TokInfo(tokVal), runLex, sm, oa, ca, ob, cb, op, cp, oq, cq, dk, wk)
import Layout
import Text.Megaparsec.Pos (SourcePos(..), unPos)

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
tests = TestList
  [
    layTest "x"                         [oa, TIden "x", ca, TEOF]
  , layTest "a\nb\nc"                   [oa, TIden "a", sm, TIden "b", sm, TIden "c", ca, TEOF]
  , layTest "do\n  a\n  b"              [oa, dk, oa, TIden "a", sm, TIden "b", ca, ca, TEOF]
  , layTest "where\n  a"                [oa, wk, oa, TIden "a", ca, ca, TEOF]
  , layTest "do\n  a\n  do\n    b"      [oa, dk, oa, TIden "a", sm, dk, oa, TIden "b", ca, ca, ca, TEOF]
  , layTest "do { a; b }"               [oa, dk, ob, TIden "a", sm, TIden "b", cb, ca, TEOF]
  , layTest "do { a\n b }"              [oa, dk, ob, TIden "a", TIden "b", cb, ca, TEOF]
  , layTest "do\n  a\n  do\n    b\n  c" [oa, dk, oa, TIden "a", sm, dk, oa, TIden "b", ca, sm, TIden "c", ca, ca, TEOF]
  , layTest "x\ndo\ny"                  [oa, TIden "x", sm, dk, oa, ca, sm, TIden "y", ca, TEOF]

  , layFail "}"
  , layFail "do { a"
  ]