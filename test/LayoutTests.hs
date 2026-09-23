module LayoutTests (tests) where

import Test.HUnit

import Lexer
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

semi    = TSemiColon
oa      = TBracket '<'
ca      = TBracket '>'
ob      = TBracket '{'
cb      = TBracket '}'
dok     = TKey "do"
wherek  = TKey "where"

tests :: Test
tests = TestList
  [
    layTest "x"       [oa, TIden "x", ca, TEOF]
  , layTest "a\nb\nc" [oa, TIden "a", semi, TIden "b", semi, TIden "c", ca, TEOF]

  , layTest "do\n  a\n  b" [oa, dok, oa, TIden "a", semi, TIden "b", ca, ca, TEOF]
  , layTest "where\n  a" [oa, wherek, oa, TIden "a", ca, ca, TEOF]
  , layTest "do\n  a\n  do\n    b" [oa, dok, oa, TIden "a", semi, dok, oa, TIden "b", ca, ca, ca, TEOF]

  , layTest "do { a; b }" [oa, dok, ob, TIden "a", semi, TIden "b", cb, ca, TEOF]

  -- implicit layout is not sensitive to indentations and newlines
  , layTest "do { a\n b }" [oa, dok, ob, TIden "a", TIden "b", cb, ca, TEOF]

  , layTest "do\n  a\n  do\n    b\n  c" [oa, dok, oa, TIden "a", semi, dok, oa, TIden "b", ca, semi, TIden "c", ca, ca, TEOF]

  -- if layout starts from same line as `do` or `where`, empty (synthetic) braces must be emitted
  , layTest "x\ndo\ny" [oa, TIden "x", semi, dok, oa, ca, semi, TIden "y", ca, TEOF]

  , layFail "}"

  , layFail "do { a"
  ]