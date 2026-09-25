module LexerTests (tests) where

import Test.HUnit

import Lexer
import Text.Megaparsec.Pos (SourcePos(..), unPos)

dropEOF :: [Tok] -> [Tok]
dropEOF ts = case last ts of
  TEOF -> init ts
  _    -> ts

lexTest :: String -> [Tok] -> Test
lexTest inp exp = TestCase $ do
  res <- return $ runLex inp
  case res of
    Left err  -> assertFailure ("lex failed on " <> show inp <> ": " <> show err)
    Right out -> dropEOF (fmap tokVal out) @?= exp

lexFail :: String -> Test
lexFail inp = TestCase $ do
  r <- return $ runLex inp
  case r of
    Left _    -> return ()
    Right out -> assertFailure ("expected lex failure on " <> show inp <> ", got " <> show (fmap tokVal out))

tests :: Test
tests = TestList $
  [
    lexTest "null"       [TLit (LNull "null")]
  , lexTest "true"       [TLit (LBool "true")]
  , lexTest "false"      [TLit (LBool "false")]
  , lexTest "0"          [TLit (LNum "0")]
  , lexTest "7"          [TLit (LNum "7")]
  , lexTest "123456"     [TLit (LNum "123456")]
  , lexTest "7.2"        [TLit (LNum "7.2")]
  , lexTest "0.5"        [TLit (LNum "0.5")]
  , lexTest "7e2"        [TLit (LNum "7e2")]
  , lexTest "7E2"        [TLit (LNum "7E2")]
  , lexTest "7e+2"       [TLit (LNum "7e+2")]
  , lexTest "7e-2"       [TLit (LNum "7e-2")]
  , lexTest "7.2e10"     [TLit (LNum "7.2e10")]
  , lexTest "\"abc\""    [TLit (LStr "abc")]
  , lexTest "'abc'"      [TLit (LStr "abc")]
  , lexTest "`abc`"      [TLit (LStr "abc")]
  , lexTest "\"\""       [TLit (LStr "")]
  , lexTest "\"a\\\"b\"" [TLit (LStr "a\"b")]
  , lexTest "\"a\\nb\""  [TLit (LStr "a\nb")]
  , lexTest "1 .2"       [TLit (LNum "1"), TSym ".", TLit (LNum "2")]

  , lexTest "x"       [TIden "x"]
  , lexTest "_x"      [TIden "_x"]
  , lexTest "x_"      [TIden "x_"]
  , lexTest "x_y"     [TIden "x_y"]
  , lexTest "x1"      [TIden "x1"]
  , lexTest "x1y2"    [TIden "x1y2"]
  , lexTest "_"       [TIden "_"]

  , lexTest "const"    [TKey "const"]
  , lexTest "where"    [TKey "where"]
  , lexTest "else"     [TKey "else"]
  , lexTest "then"     [TKey "then"]
  , lexTest "let"      [TKey "let"]
  , lexTest "fn"       [TKey "fn"]
  , lexTest "if"       [TKey "if"]
  , lexTest "do"       [TKey "do"]

  , lexTest "doo"      [TIden "doo"]
  , lexTest "lets"     [TIden "lets"]
  , lexTest "iffy"     [TIden "iffy"]
  , lexTest "constant" [TIden "constant"]
  , lexTest "fnord"    [TIden "fnord"]
  , lexTest "wherein"  [TIden "wherein"]
  , lexTest "typeofx"  [TIden "typeofx"]
  , lexTest "if_"      [TIden "if_"]

  , lexTest "if(x)"    [TKey "if", op, TIden "x", cp]

  , lexTest "a;b" [TIden "a", sm, TIden "b"]

  , lexTest "  x"          [TIden "x"]
  , lexTest "\tx"          [TIden "x"]
  , lexTest "x // hi"      [TIden "x"]
  , lexTest "x // hi\nx"   [TIden "x", TIden "x"]

  , lexTest "x = 1"         [TIden "x", TSym "=", TLit (LNum "1")]
  , lexTest "let x = 1"     [TKey "let", TIden "x", TSym "=", TLit (LNum "1")]
  , lexTest "fn x -> x"     [TKey "fn", TIden "x", TSym "->", TIden "x"]
  , lexTest "f(x, y)"       [TIden "f", op, TIden "x", TSym ",", TIden "y", cp]
  , lexTest "[1, 2, 3]"     [oq, TLit (LNum "1"), TSym ",", TLit (LNum "2"), TSym ",", TLit (LNum "3"), cq]
  , lexTest "a.b.c"         [TIden "a", TSym ".", TIden "b", TSym ".", TIden "c"]
  , lexTest "a ? b : c"     [TIden "a", TSym "?", TIden "b", TSym ":", TIden "c"]
  , lexTest "a + b * c"     [TIden "a", TSym "+", TIden "b", TSym "*", TIden "c"]

  , lexTest "fn x -> x"   [TKey "fn", TIden "x", TSym "->", TIden "x"]
  , lexTest "f :: Int"    [TIden "f", TSym "::", TIden "Int"]
  , lexTest "a == b"      [TIden "a", TSym "==", TIden "b"]
  , lexTest "a = = b"     [TIden "a", TSym "=", TSym "=", TIden "b"]
  , lexTest "a >= b"      [TIden "a", TSym ">=", TIden "b"]
  , lexTest "x = 1"       [TIden "x", TSym "=", TLit (LNum "1")]
  , lexTest "a + b"       [TIden "a", TSym "+", TIden "b"]
  , lexTest "a - b"       [TIden "a", TSym "-", TIden "b"]
  , lexTest "a * b"       [TIden "a", TSym "*", TIden "b"]
  , lexTest "a / b"       [TIden "a", TSym "/", TIden "b"]
  , lexTest "{ a : 1 }"   [ob, TIden "a", TSym ":", TLit (LNum "1"), cb]
  , lexTest "f(a, b)"     [TIden "f", op, TIden "a", TSym ",", TIden "b", cp]
  , lexTest "a.b"         [TIden "a", TSym ".", TIden "b"]
  , lexTest "a < b"       [TIden "a", TSym "<", TIden "b"]
  , lexTest "a > b"       [TIden "a", TSym ">", TIden "b"]
  , lexTest "a ? b : c"   [TIden "a", TSym "?", TIden "b", TSym ":", TIden "c"]

  , lexTest "let f = fn x -> x + 1" [TKey "let", TIden "f", TSym "=", TKey "fn", TIden "x", TSym "->", TIden "x", TSym "+", TLit (LNum "1")]

  , lexTest "f(g(x), [1, 2], { a: 1 })"
      [TIden "f",
      op,
        TIden "g", op, TIden "x", cp, TSym ",",
        oq, TLit (LNum "1"), TSym ",", TLit (LNum "2"), cq, TSym ",",
        ob, TIden "a", TSym ":", TLit (LNum "1"), cb,
      cp]

  , lexTest "a.b(c).d[0]" [TIden "a", TSym ".", TIden "b", op, TIden "c", cp, TSym ".", TIden "d", oq, TLit (LNum "0"), cq]

  , lexTest "x = a ? b : c ? d : e"
      [TIden "x", TSym "=", TIden "a", TSym "?", TIden "b", TSym ":", TIden "c", TSym "?", TIden "d", TSym ":", TIden "e"]

  , lexTest "f :: Int -> String -> Bool"
      [TIden "f", TSym "::", TIden "Int", TSym "->", TIden "String", TSym "->", TIden "Bool"]

  , lexTest "// leading comment\nlet x = 1 // trailing\nlet y = 2"
      [TKey "let", TIden "x", TSym "=", TLit (LNum "1"),
       TKey "let", TIden "y", TSym "=", TLit (LNum "2")]

  , lexTest "\"a\" + \"b\" == `c`"
      [TLit (LStr "a"), TSym "+", TLit (LStr "b"), TSym "==", TLit (LStr "c")]

  , lexTest "do { let x = 1; x }" [TKey "do", ob, TKey "let", TIden "x", TSym "=", TLit (LNum "1"), sm, TIden "x", cb]

  , lexTest "// just a comment"  []
  , lexTest "x // c1\n// c2\ny"  [TIden "x", TIden "y"]

  , lexFail "\"unterminated"
  , lexFail "'unterminated"

  , TestCase $ do
      let Right ts = runLex "x\ny"
      fmap (unPos . sourceLine . tokStart) ts @?= [1, 2, 2]

  , TestCase $ do
      let Right ts = runLex "  x"
      fmap (unPos . sourceColumn . tokStart) (take 1 ts) @?= [3]
  ]
  <>
  [
    lexTest "type"       [TKey "type"]
  , lexTest "forall"     [TKey "forall"]
  , lexTest "typed"      [TIden "typed"]
  , lexTest "forallx"    [TIden "forallx"]
  , lexTest "=>"         [TSym "=>"]
  , lexTest "b?: B"      [TIden "b", TSym "?", TSym ":", TIden "B"]
  , lexTest "T[]"        [TIden "T", oq, cq]
  , lexTest "forall T. fn(x: T) => T" [TKey "forall", TIden "T", TSym ".", TKey "fn", op, TIden "x", TSym ":", TIden "T", cp, TSym "=>", TIden "T"]
  ]