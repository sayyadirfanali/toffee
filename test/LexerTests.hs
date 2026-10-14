module LexerTests (tests) where

import Prelude hiding (exp)

import Test.HUnit
import Lexer
import Text.Megaparsec.Pos (SourcePos(..), unPos)

lexWith :: String -> ([TokInfo] -> Assertion) -> Test
lexWith inp k = TestCase $ case runLex inp of
  Left err  -> assertFailure ("lex failed on " <> show inp <> ": " <> show err)
  Right out -> k out

ok :: String -> [Tok] -> Test
ok inp exp = lexWith inp $ \out -> filter (/= TEOF) (fmap tokVal out) @?= exp

lexFail :: String -> Test
lexFail inp = TestCase $ case runLex inp of
  Left _    -> return ()
  Right out -> assertFailure ("expected lex failure on " <> show inp <> ", got " <> show (fmap tokVal out))

lineCol :: TokInfo -> (Int, Int)
lineCol t = (unPos (sourceLine (tokStart t)), unPos (sourceColumn (tokStart t)))

idens :: String -> [Tok]
idens = fmap TIden . words

num :: String -> Tok
num = TLit . LNum

str :: String -> Tok
str = TLit . LStr

tests :: Test
tests = TestList $
  concat
    [ concat [ literals, numbers, strings, identifiers, keywords, symbols , brackets, privates, regexes, whitespace, composite ]
    , generated
    , fmap lexFail fails
    , positions
    ]

generated :: [Test]
generated =
     [ ok k [TKey k] | k <- keys ]
  <> [ ok (k <> s) [TIden (k <> s)] | k <- keys, s <- ["x", "1", "_"] ]
  <> [ ok (k <> "(") [TKey k, op] | k <- keys ]
  <> [ ok s [TSym s] | s <- ops ]
  <> [ ok ("a" <> s <> "b") [TIden "a", TSym s, TIden "b"] | s <- ops ]
  <> [ ok ("a " <> s <> " b") [TIden "a", TSym s, TIden "b"] | s <- ops ]

literals :: [Test]
literals =
  [ ok "null" [TLit (LNull "null")]
  , ok "true" [TLit (LBool "true")]
  , ok "false" [TLit (LBool "false")]
  , ok "nullable trueish falsey null1 true_ Null" (idens "nullable trueish falsey null1 true_ Null")
  ]

numbers :: [Test]
numbers =
  fmap (\s -> ok s [num s])
    (words "0 7 123456 7.2 0.5 0.0 7e2 7E2 7e+2 7e-2 7.2e10 1.5E-3 0x1F 0xff 0XFF 0o17 0O17 0b101 0B101 123n 0x1Fn 0b101n 1_000 1_000_000 0xFF_FF 1_000.5")
  <>
  [ ok "1 .2" [num "1", TSym ".", num "2"]
  , ok ".5" [TSym ".", num "5"]
  , ok "-1" [TSym "-", num "1"]
  , ok "1-1" [num "1", TSym "-", num "1"]
  , ok "1n + 2n" [num "1n", TSym "+", num "2n"]
  ]

strings :: [Test]
strings =
  [ ok "\"abc\"" [str "abc"]
  , ok "'abc'" [str "abc"]
  , ok "`abc`" [str "abc"] 
  , ok "\"\" '' ``" [str "", str "", str ""]
  , ok "\"a b  c\"" [str "a b  c"]
  , ok "\"a\\\"b\"" [str "a\"b"]
  , ok "'a\\'b'" [str "a'b"]
  , ok "\"a\\nb\\tc\\\\d\"" [str "a\nb\tc\\d"]
  , ok "'a\"b' \"a'b\" `a\"b'c`" [str "a\"b", str "a'b", str "a\"b'c"]
  , ok "\"a + b\" \"x // y\"" [str "a + b", str "x // y"]
  , ok "\"a\" \"b\"" [str "a", str "b"]
  , ok "\"a\"\"b\"" [str "a", str "b"]
  , ok "\"a\\qb\"" [str "aqb"]
  , ok "\"\\x41\"" [str "A"]
  , ok "\"\\x4142\"" [str "A42"]
  , ok "\"\\u0041\"" [str "A"]
  , ok "\"\\u{1F600}\"" [str "\128512"]
  , ok "\"\\0\"" [str "\0"]
  , ok "\"\\b\\f\\v\\r\"" [str "\b\f\v\r"]
  ]

identifiers :: [Test]
identifiers =
  fmap (\s -> ok s [TIden s]) (words "x _x x_ x_y x1 x1y2 _ $ __ $$ X FooBar camelCase café") <> [ ok "a b" (idens "a b") ]

keywords :: [Test]
keywords =
  fmap (\w -> ok w [TIden w]) (keycon <> keysuff)
  <>
  [ ok "typeof x" [TKey "typeof", TIden "x"]
  , ok "raw\"x\"" [TIden "raw", str "x"]
  ]
  where
    keycon = words "undefined protected arguments function default static switch yield async this from case and set get of meta type as forall interface satisfies raw implements readonly abstract override"
    keysuff = words "types doo lets iffy constant fnord wherein newer assume awaiting voidable elsewhere thenceforth typeofx"

symbols :: [Test]
symbols =
  [ ok "...." [TSym "...", TSym "."]
  , ok ".." [TSym ".", TSym "."]
  , ok "?.." [TSym "?.", TSym "."]
  , ok "?.5" [TSym "?.", num "5"]
  , ok "???" [TSym "??", TSym "?"]
  , ok "a?b" [TIden "a", TSym "?", TIden "b"]
  , ok "***" [TSym "**", TSym "*"]
  , ok "**==" [TSym "**=", TSym "="]
  , ok "===" [TSym "==", TSym "="]
  , ok "!==" [TSym "!=", TSym "="]
  , ok "==>" [TSym "==", TSym ">"]
  , ok "=>=" [TSym "=>", TSym "="]
  , ok " -->" [TSym "-", TSym "->"]
  , ok "<<<" [TSym "<<", TSym "<"]
  , ok ">>>>" [TSym ">>>", TSym ">"]
  , ok ">>>>=" [TSym ">>>", TSym ">="]
  , ok "&&&" [TSym "&&", TSym "&"]
  , ok "&&==" [TSym "&&=", TSym "="]
  , ok "|||" [TSym "||", TSym "|"]
  , ok ":::" [TSym "::", TSym ":"]
  , ok "!!x" [TSym "!", TSym "!", TIden "x"]
  , ok " --x" [TSym "-", TSym "-", TIden "x"]
  , ok "a-=-1" [TIden "a", TSym "-=", TSym "-", num "1"]
  , ok "a<b>c" [TIden "a", TSym "<", TIden "b", TSym ">", TIden "c"]
  , ok "a<-1" [TIden "a", TSym "<", TSym "-", num "1"]
  , ok "x!=1" [TIden "x", TSym "!=", num "1"]
  , ok "x! = 1" [TIden "x", TSym "!", TSym "=", num "1"]
  , ok "a = = b" [TIden "a", TSym "=", TSym "=", TIden "b"]
  ]

brackets :: [Test]
brackets =
  [ ok "()[]{}" [op, cp, oq, cq, ob, cb]
  , ok "([{}])" [op, oq, ob, cb, cq, cp]
  , ok "(a)" [op, TIden "a", cp]
  , ok "T[]" [TIden "T", oq, cq]
  , ok "<>" [TSym "<", TSym ">"]
  , ok ";" [sm]
  , ok ";;" [sm, sm]
  , ok "a;b" [TIden "a", sm, TIden "b"]
  , ok "a ; b" [TIden "a", sm, TIden "b"]
  ]

privates :: [Test]
privates =
  [ ok "#x" [TPriv "x"]
  , ok "#if #_a1 #$" [TPriv "if", TPriv "_a1", TPriv "$"]
  , ok "a.#b" [TIden "a", TSym ".", TPriv "b"]
  , ok "#x in o" [TPriv "x", TKey "in", TIden "o"]
  ]

regexes :: [Test]
regexes =
  [ ok "r\"[a-z]+\"" [TLit (LReg "[a-z]+" "")]
  , ok "r\"\\d+\"" [TLit (LReg "\\d+" "")]
  , ok "r\"\"" [TLit (LReg "" "")]
  , ok "r\"a\\\"b\"" [TLit (LReg "a\"b" "")]
  , ok "r\"\\d+\"g" [TLit (LReg "\\d+" "g")]
  , ok "r\"[a-z]\"gi" [TLit (LReg "[a-z]" "gi")]
  , ok "r\"x\"gimsuydv" [TLit (LReg "x" "gimsuydv")]
  , ok "r\"x\"then" [TLit (LReg "x" ""), TKey "then"]
  , ok "r\"x\"g_" [TLit (LReg "x" "g"), TIden "_"]
  , ok "r" [TIden "r"]
  , ok "r + 1" [TIden "r", TSym "+", num "1"]
  , ok "r \"x\"" [TIden "r", str "x"]
  , ok "r.test(s)" [TIden "r", TSym ".", TIden "test", op, TIden "s", cp]
  , ok "R\"x\"" [TIden "R", str "x"]
  ]

whitespace :: [Test]
whitespace =
  [ ok "" []
  , ok "   " []
  , ok "\n\n" []
  , ok "\t" []
  , ok "  x" [TIden "x"]
  , ok "\tx" [TIden "x"]
  , ok "x  " [TIden "x"]
  , ok "a\nb" (idens "a b")
  , ok "a\n\n  b" (idens "a b")
  , ok "// just a comment" []
  , ok "x // hi" [TIden "x"]
  , ok "x//hi" [TIden "x"]
  , ok "x // hi\nx" (idens "x x")
  , ok "// c\nx" [TIden "x"]
  , ok "x // c1\n// c2\ny" (idens "x y")
  , ok "// a + b \"not a string" []
  , ok "a / b" [TIden "a", TSym "/", TIden "b"]
  ]

composite :: [Test]
composite =
  [ ok "let f = fn(x) => x + 1" [TKey "let", TIden "f", TSym "=", TKey "fn", op, TIden "x", cp, TSym "=>", TIden "x", TSym "+", num "1"]

  , ok  "f(g(x), [1, 2], { a: 1 })"
      [TIden "f", op, TIden "g", op, TIden "x", cp, TSym ",", oq, num "1", TSym ",", num "2", cq, TSym "," , ob, TIden "a", TSym ":", num "1", cb, cp]

  , ok "a.b(c).d[0]" [TIden "a", TSym ".", TIden "b", op, TIden "c", cp, TSym ".", TIden "d", oq, num "0", cq] 

  , ok "a?.b?.(c)?.[0]" [TIden "a", TSym "?.", TIden "b", TSym "?.", op, TIden "c", cp, TSym "?.", oq, num "0", cq] 

  , ok "x ??= y ?? z" [TIden "x", TSym "??=", TIden "y", TSym "??", TIden "z"]

  , ok "new Foo(...args)" [TKey "new", TIden "Foo", op, TSym "...", TIden "args", cp]

  , ok "if a then b else c" [TKey "if", TIden "a", TKey "then", TIden "b", TKey "else", TIden "c"]

  , ok "do { let x = 1; x }" [TKey "do", ob, TKey "let", TIden "x", TSym "=", num "1", sm, TIden "x", cb]

  , ok "a instanceof B" [TIden "a", TKey "instanceof", TIden "B"]
  
  , ok "await f()" [TKey "await", TIden "f", op, cp]

  , ok "void 0" [TKey "void", num "0"]

  , ok "this.x" [TIden "this", TSym ".", TIden "x"]

  , ok "super()" [TIden "super", op, cp]

  , ok "x == undefined" [TIden "x", TSym "==", TIden "undefined"]

  , ok "s.match(r\"\\d+\"g)" [TIden "s", TSym ".", TIden "match", op, TLit (LReg "\\d+" "g"), cp]

  , ok  "f :: Int -> String -> Bool" [TIden "f", TSym "::", TIden "Int", TSym "->", TIden "String", TSym "->", TIden "Bool"] 

  , ok "\"a\" + \"b\" == `c`" [str "a", TSym "+", str "b", TSym "==", str "c"]

  , ok  "// leading comment\nlet x = 1 // trailing\nlet y = 2" [TKey "let", TIden "x", TSym "=", num "1", TKey "let", TIden "y", TSym "=", num "2"] 
  ]

fails :: [String]
fails =
  [ "\"unterminated"
  , "'unterminated"
  , "`unterminated"
  , "#"
  , "@"
  , "~"
  , "\\"
  , "\"\\x4\""
  , "\"\\u041\""
  , "r\"abc"
  ]

positions :: [Test]
positions =
  [ lexWith "x\ny"        $ \r -> fmap (fst . lineCol) r @?= [1, 2, 2]
  , lexWith "  x"         $ \r -> fmap (snd . lineCol) (take 1 r) @?= [3]
  , lexWith "a\n  b"      $ \r -> fmap lineCol r @?= [(1, 1), (2, 3), (2, 4)]
  , lexWith "x\n"         $ \r -> fmap lineCol r @?= [(1, 1), (2, 1)]
  , lexWith "  // c\n  x" $ \r -> fmap lineCol (take 1 r) @?= [(2, 3)]
  , lexWith "\tx"         $ \r -> fmap lineCol (take 1 r) @?= [(1, 9)]
  , lexWith ">>>= typeof" $ \r -> fmap tokStr (take 2 r) @?= [">>>=", "typeof"]
  , lexWith "r\"\\d+\"gi" $ \r -> fmap tokStr (take 1 r) @?= ["r\"\\d+\"gi"]
  ]