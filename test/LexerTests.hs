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

lineCol :: TokInfo -> (Int, Int)
lineCol t = (unPos (sourceLine (tokStart t)), unPos (sourceColumn (tokStart t)))

tests :: Test
tests = TestList $ concat
  [ literals
  , identifiers
  , notKeywords
  , keywords
  , symbols
  , maximalMunch
  , brackets
  , whitespace
  , composite
  , fails
  , positions
  ]

literals :: [Test]
literals =
  [ lexTest "null" [TLit (LNull "null")]
  , lexTest "true" [TLit (LBool "true")]
  , lexTest "false" [TLit (LBool "false")]

  , lexTest "nullable" [TIden "nullable"]
  , lexTest "trueish" [TIden "trueish"]
  , lexTest "falsey" [TIden "falsey"]
  , lexTest "null1" [TIden "null1"]
  , lexTest "true_" [TIden "true_"]
  , lexTest "Null" [TIden "Null"]

  , lexTest "0" [TLit (LNum "0")]
  , lexTest "7" [TLit (LNum "7")]
  , lexTest "123456" [TLit (LNum "123456")]
  , lexTest "007" [TLit (LNum "007")]
  , lexTest "7.2" [TLit (LNum "7.2")]
  , lexTest "0.5" [TLit (LNum "0.5")]
  , lexTest "0.0" [TLit (LNum "0.0")]
  , lexTest "7e2" [TLit (LNum "7e2")]
  , lexTest "7E2" [TLit (LNum "7E2")]
  , lexTest "7e+2" [TLit (LNum "7e+2")]
  , lexTest "7e-2" [TLit (LNum "7e-2")]
  , lexTest "7.2e10" [TLit (LNum "7.2e10")]
  , lexTest "1.5E-3" [TLit (LNum "1.5E-3")]
  , lexTest "1 .2" [TLit (LNum "1"), TSym ".", TLit (LNum "2")]
  , lexTest ".5" [TSym ".", TLit (LNum "5")]
  , lexTest "-1" [TSym "-", TLit (LNum "1")]
  , lexTest "1-1" [TLit (LNum "1"), TSym "-", TLit (LNum "1")]

  , lexTest "\"abc\"" [TLit (LStr "abc")]
  , lexTest "'abc'" [TLit (LStr "abc")]
  , lexTest "`abc`" [TLit (LStr "abc")]
  , lexTest "\"\"" [TLit (LStr "")]
  , lexTest "''" [TLit (LStr "")]
  , lexTest "``" [TLit (LStr "")]
  , lexTest "\"a b  c\"" [TLit (LStr "a b  c")]

  , lexTest "\"a\\\"b\"" [TLit (LStr "a\"b")]
  , lexTest "'a\\'b'" [TLit (LStr "a'b")]
  , lexTest "\"a\\nb\"" [TLit (LStr "a\nb")]
  , lexTest "\"a\\tb\"" [TLit (LStr "a\tb")]
  , lexTest "\"a\\\\b\"" [TLit (LStr "a\\b")]

  , lexTest "'a\"b'" [TLit (LStr "a\"b")]
  , lexTest "\"a'b\"" [TLit (LStr "a'b")]
  , lexTest "`a\"b'c`" [TLit (LStr "a\"b'c")]

  , lexTest "\"a + b\"" [TLit (LStr "a + b")]
  , lexTest "\"x // y\"" [TLit (LStr "x // y")]

  , lexTest "\"a\" \"b\"" [TLit (LStr "a"), TLit (LStr "b")]
  , lexTest "\"a\"\"b\"" [TLit (LStr "a"), TLit (LStr "b")]
  ]

identifiers :: [Test]
identifiers =
  [ lexTest "x" [TIden "x"]
  , lexTest "_x" [TIden "_x"]
  , lexTest "x_" [TIden "x_"]
  , lexTest "x_y" [TIden "x_y"]
  , lexTest "x1" [TIden "x1"]
  , lexTest "x1y2" [TIden "x1y2"]
  , lexTest "_" [TIden "_"]
  , lexTest "$" [TIden "$"]
  , lexTest "__" [TIden "__"]
  , lexTest "$$" [TIden "$$"]
  , lexTest "X" [TIden "X"]
  , lexTest "FooBar" [TIden "FooBar"]
  , lexTest "camelCase" [TIden "camelCase"]
  , lexTest "café" [TIden "café"]
  , lexTest "a b" [TIden "a", TIden "b"]
  ]

notKeywords :: [Test]
notKeywords =
  [ lexTest w [TIden w]
  | w <- words "of this case from async super yield switch function undefined"
  ]

keywords :: [Test]
keywords =
  [ lexTest k [TKey k] | k <- keys ]
  <>
  [ lexTest (k <> s) [TIden (k <> s)] | k <- keys, s <- ["x", "1", "_"] ]
  <>
  [ lexTest (k <> "(") [TKey k, op] | k <- keys ]
  <>
  [ lexTest w [TIden w]
  | w <- words "types doo lets iffy constant fnord wherein newer assume async awaiting voidable elsewhere thenceforth rawx forallx typed"
  ]
  <>
  [ lexTest "type" [TKey "type"]
  , lexTest "typeof" [TKey "typeof"]
  , lexTest "typeof x" [TKey "typeof", TIden "x"]
  , lexTest "as" [TKey "as"]
  , lexTest "raw\"x\"" [TKey "raw", TLit (LStr "x")]
  ]

symbols :: [Test]
symbols =
  [ lexTest s [TSym s] | s <- ops ]
  <>
  [ lexTest ("a" <> s <> "b") [TIden "a", TSym s, TIden "b"] | s <- ops ]
  <>
  [ lexTest ("a " <> s <> " b") [TIden "a", TSym s, TIden "b"] | s <- ops ]

maximalMunch :: [Test]
maximalMunch =
  [ lexTest "...." [TSym "...", TSym "."]
  , lexTest ".." [TSym ".", TSym "."]
  , lexTest "?.." [TSym "?.", TSym "."]
  , lexTest "?.5" [TSym "?.", TLit (LNum "5")]
  , lexTest "???" [TSym "??", TSym "?"]
  , lexTest "***" [TSym "**", TSym "*"]
  , lexTest "**==" [TSym "**=", TSym "="]
  , lexTest "===" [TSym "==", TSym "="]
  , lexTest "!==" [TSym "!=", TSym "="]
  , lexTest "==>" [TSym "==", TSym ">"]
  , lexTest "=>=" [TSym "=>", TSym "="]
  , lexTest "-->" [TSym "-", TSym "->"]
  , lexTest "<<<" [TSym "<<", TSym "<"]
  , lexTest ">>>>" [TSym ">>>", TSym ">"]
  , lexTest ">>>>=" [TSym ">>>", TSym ">="]
  , lexTest "&&&" [TSym "&&", TSym "&"]
  , lexTest "&&==" [TSym "&&=", TSym "="]
  , lexTest "|||" [TSym "||", TSym "|"]
  , lexTest ":::" [TSym "::", TSym ":"]
  , lexTest "!!x" [TSym "!", TSym "!", TIden "x"]
  , lexTest "--x" [TSym "-", TSym "-", TIden "x"]
  , lexTest "a-=-1" [TIden "a", TSym "-=", TSym "-", TLit (LNum "1")]
  , lexTest "a<b>c" [TIden "a", TSym "<", TIden "b", TSym ">", TIden "c"]
  , lexTest "x!=1" [TIden "x", TSym "!=", TLit (LNum "1")]
  , lexTest "x! = 1" [TIden "x", TSym "!", TSym "=", TLit (LNum "1")]
  , lexTest "a = = b" [TIden "a", TSym "=", TSym "=", TIden "b"]
  ]

brackets :: [Test]
brackets =
  [ lexTest "()[]{}" [op, cp, oq, cq, ob, cb]
  , lexTest "([{}])" [op, oq, ob, cb, cq, cp]
  , lexTest "(a)" [op, TIden "a", cp]
  , lexTest "T[]" [TIden "T", oq, cq]
  , lexTest "<>" [TSym "<", TSym ">"]
  , lexTest ";" [sm]
  , lexTest ";;" [sm, sm]
  , lexTest "a;b" [TIden "a", sm, TIden "b"]
  , lexTest "a ; b" [TIden "a", sm, TIden "b"]
  ]

whitespace :: [Test]
whitespace =
  [ lexTest "" []
  , lexTest "   " []
  , lexTest "\n\n" []
  , lexTest "\t" []
  , lexTest "  x" [TIden "x"]
  , lexTest "\tx" [TIden "x"]
  , lexTest "x  " [TIden "x"]
  , lexTest "a\nb" [TIden "a", TIden "b"]
  , lexTest "a\n\n  b" [TIden "a", TIden "b"]
  , lexTest "// just a comment" []
  , lexTest "x // hi" [TIden "x"]
  , lexTest "x//hi" [TIden "x"]
  , lexTest "x // hi\nx" [TIden "x", TIden "x"]
  , lexTest "// c\nx" [TIden "x"]
  , lexTest "x // c1\n// c2\ny" [TIden "x", TIden "y"]
  , lexTest "// a + b \"not a string" []
  , lexTest "a / b" [TIden "a", TSym "/", TIden "b"]
  ]

composite :: [Test]
composite =
  [ lexTest "let f = fn(x) => x + 1"
      [TKey "let", TIden "f", TSym "=", TKey "fn", op, TIden "x", cp, TSym "=>", TIden "x", TSym "+", TLit (LNum "1")]

  , lexTest "f(g(x), [1, 2], { a: 1 })"
      [TIden "f", op,
        TIden "g", op, TIden "x", cp, TSym ",",
        oq, TLit (LNum "1"), TSym ",", TLit (LNum "2"), cq, TSym ",",
        ob, TIden "a", TSym ":", TLit (LNum "1"), cb,
      cp]

  , lexTest "a.b(c).d[0]" [TIden "a", TSym ".", TIden "b", op, TIden "c", cp, TSym ".", TIden "d", oq, TLit (LNum "0"), cq]

  , lexTest "a?.b?.(c)?.[0]"
      [TIden "a", TSym "?.", TIden "b", TSym "?.", op, TIden "c", cp, TSym "?.", oq, TLit (LNum "0"), cq]

  , lexTest "x ??= y ?? z" [TIden "x", TSym "??=", TIden "y", TSym "??", TIden "z"]

  , lexTest "new Foo(...args)" [TKey "new", TIden "Foo", op, TSym "...", TIden "args", cp]

  , lexTest "if a then b else c" [TKey "if", TIden "a", TKey "then", TIden "b", TKey "else", TIden "c"]

  , lexTest "do { let x = 1; x }" [TKey "do", ob, TKey "let", TIden "x", TSym "=", TLit (LNum "1"), sm, TIden "x", cb]

  , lexTest "a instanceof B" [TIden "a", TKey "instanceof", TIden "B"]

  , lexTest "await f()" [TKey "await", TIden "f", op, cp]

  , lexTest "void 0" [TKey "void", TLit (LNum "0")]

  , lexTest "x as T" [TIden "x", TKey "as", TIden "T"]

  , lexTest "x satisfies T" [TIden "x", TKey "satisfies", TIden "T"]

  , lexTest "type Pair(A, B) = [A, B]"
      [TKey "type", TIden "Pair", op, TIden "A", TSym ",", TIden "B", cp, TSym "=", oq, TIden "A", TSym ",", TIden "B", cq]

  , lexTest "forall T. fn(x: T) => T"
      [TKey "forall", TIden "T", TSym ".", TKey "fn", op, TIden "x", TSym ":", TIden "T", cp, TSym "=>", TIden "T"]

  , lexTest "{ a: A, b?: B }"
      [ob, TIden "a", TSym ":", TIden "A", TSym ",", TIden "b", TSym "?", TSym ":", TIden "B", cb]

  , lexTest "raw \"Partial<User>\"" [TKey "raw", TLit (LStr "Partial<User>")]

  , lexTest "f :: Int -> String -> Bool"
      [TIden "f", TSym "::", TIden "Int", TSym "->", TIden "String", TSym "->", TIden "Bool"]

  , lexTest "\"a\" + \"b\" == `c`"
      [TLit (LStr "a"), TSym "+", TLit (LStr "b"), TSym "==", TLit (LStr "c")]

  , lexTest "// leading comment\nlet x = 1 // trailing\nlet y = 2"
      [TKey "let", TIden "x", TSym "=", TLit (LNum "1"),
       TKey "let", TIden "y", TSym "=", TLit (LNum "2")]
  ]

fails :: [Test]
fails =
  [ lexFail "\"unterminated"
  , lexFail "'unterminated"
  , lexFail "`unterminated"
  , lexFail "\"a\\qb\""
  , lexFail "#"
  , lexFail "@"
  , lexFail "~"
  , lexFail "\\"
  ]

positions :: [Test]
positions =
  [ TestCase $ do
      let Right ts = runLex "x\ny"
      fmap (unPos . sourceLine . tokStart) ts @?= [1, 2, 2]

  , TestCase $ do
      let Right ts = runLex "  x"
      fmap (unPos . sourceColumn . tokStart) (take 1 ts) @?= [3]

  , TestCase $ do
      let Right ts = runLex "a\n  b"
      fmap lineCol ts @?= [(1, 1), (2, 3), (2, 4)]

  , TestCase $ do
      let Right ts = runLex "x\n"
      fmap lineCol ts @?= [(1, 1), (2, 1)]

  , TestCase $ do
      let Right ts = runLex "  // c\n  x"
      fmap lineCol (take 1 ts) @?= [(2, 3)]

  , TestCase $ do
      let Right ts = runLex "\tx"
      fmap lineCol (take 1 ts) @?= [(1, 9)]

  , TestCase $ do
      let Right ts = runLex ">>>= typeof"
      fmap tokStr (take 2 ts) @?= [">>>=", "typeof"]
  ]