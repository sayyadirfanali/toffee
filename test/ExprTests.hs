module ExprTests (tests) where

import Prelude hiding (exp)

import Test.HUnit
import Data.List (isInfixOf)
import qualified Data.List.NonEmpty as NE
import Text.Megaparsec (errorBundlePretty, bundleErrors, parseErrorTextPretty, sourceColumn, sourceLine, unPos)

import Expr
import Layout (layout)
import Lexer (TokInfo, runLex)
import Parser (Parser, eofP, runParse)
import Render (render)

parseWith :: Parser a -> [TokInfo] -> Either String a
parseWith p ts = case runParse p ts of
  Left b  -> Left (concatMap parseErrorTextPretty (NE.toList (bundleErrors b)))
  Right x -> Right x

parseExpr :: String -> Either String Expr
parseExpr s = case runLex s of
  Left e   -> Left ("lex error: " <> errorBundlePretty e)
  Right ts -> parseWith (exprE <* eofP) ts

parseTop :: String -> Either String [Expr]
parseTop s = case runLex s of
  Left e   -> Left ("lex error: " <> errorBundlePretty e)
  Right ts -> case layout ts of
    Left e   -> Left ("layout error: " <> show e)
    Right ys -> parseWith programP ys

ok :: String -> String -> Test
ok inp exp = TestCase $ case parseExpr inp of
  Left err -> assertFailure ("parse failed on " <> show inp <> ": " <> err)
  Right e  -> render e @?= exp

bad :: String -> Test
bad inp = TestCase $ case parseExpr inp of
  Left _  -> return ()
  Right e -> assertFailure ("expected failure on " <> show inp <> ", got " <> render e)

-- fails, and the error text mentions the given fragment
badMsg :: String -> String -> Test
badMsg inp frag = TestCase $ case parseExpr inp of
  Left err -> assertBool ("error on " <> show inp <> " lacks " <> show frag <> ": " <> err) (frag `isInfixOf` err)
  Right e  -> assertFailure ("expected failure on " <> show inp <> ", got " <> render e)

top :: String -> [String] -> Test
top inp exp = TestCase $ case parseTop inp of
  Left err -> assertFailure ("failed on " <> show inp <> ": " <> err)
  Right es -> fmap render es @?= exp

badTop :: String -> Test
badTop inp = TestCase $ case parseTop inp of
  Left _   -> return ()
  Right es -> assertFailure ("expected failure on " <> show inp <> ", got " <> show (fmap render es))

offsets :: Expr -> (Int, Int)
offsets e = (e.exSpan.spStartOff, e.exSpan.spEndOff)

-- character offsets of the whole expression
spanOk :: String -> (Int, Int) -> Test
spanOk inp exp = TestCase $ case parseExpr inp of
  Left err -> assertFailure ("parse failed on " <> show inp <> ": " <> err)
  Right e  -> offsets e @?= exp

-- character offsets of each top-level expression, after layout
topSpans :: String -> [(Int, Int)] -> Test
topSpans inp exp = TestCase $ case parseTop inp of
  Left err -> assertFailure ("failed on " <> show inp <> ": " <> err)
  Right es -> fmap offsets es @?= exp

-- line and column where the expression starts
startOk :: String -> (Int, Int) -> Test
startOk inp exp = TestCase $ case parseExpr inp of
  Left err -> assertFailure ("parse failed on " <> show inp <> ": " <> err)
  Right e  -> (unPos (sourceLine e.exSpan.spStart), unPos (sourceColumn e.exSpan.spStart)) @?= exp

tests :: Test
tests = TestList $ concat
  [ atoms, arrays, objects, objectMethods, literalNames, postfix, privateMembers, spreadArgs, newTests
  , unary, incDec, binary, precedence, mixing, nonAssoc, ins, deletes
  , lambdas, params, asyncs, trailingCommas
  , conditionals, tries, throws, assignment, declarations, patterns, keywordBindings
  , classes, reservedMembers, imports, exports, blocks, loops, grouping
  , tsGone, topLevel, blockEnds, topFailures, spans
  ]

atoms :: [Test]
atoms =
  [ ok s s | s <- ["x", "_x", "42", "1.5", "\"a\"", "true", "false", "null", "r\"\\d+\"g", ".5", "0x1F", "1_000n"] ] <> [ ok "'a'" "\"a\"" ]

arrays :: [Test]
arrays =
  [ ok "[]" "[]"
  , ok "[1]" "[1]"
  , ok "[1, 2]" "[1 2]"
  , ok "[1, ...xs, 2]" "[1 ...xs 2]"
  , ok "[...xs]" "[...xs]"
  , ok "[[1], [2]]" "[[1] [2]]"
  , ok "[a + b]" "[(+ a b)]"
  , ok "[fn(x) => x]" "[(fn (x) x)]"
  , ok "[a = 1]" "[(= a 1)]"

  , bad "[1, 2"
  , bad "[,]"
  , bad "[...]"
  ]

objects :: [Test]
objects =
  [ ok "{}" "{}"
  , ok "{ a: 1 }" "{a:1}"
  , ok "{ a }" "{a}"
  , ok "{ ...o }" "{...o}"
  , ok "{ [k]: v }" "{[k]:v}"
  , ok "{ [a + b]: 1 }" "{[(+ a b)]:1}"
  , ok "{ if: 1 }" "{if:1}"
  , ok "{ [\"a-b\"]: 1 }" "{[\"a-b\"]:1}"
  , ok "{ [0]: x }" "{[0]:x}"
  , ok "{ new: 1, if: 2 }" "{new:1 if:2}"
  , ok "{ a: { b: 1 } }" "{a:{b:1}}"
  , ok "{ a: 1, b, ...c, [d]: e }" "{a:1 b ...c [d]:e}"
  , ok "{ a = 1 }" "{a=1}"
  , ok "{ a: b = 1 }" "{a:(= b 1)}"

  -- string and number keys
  , ok "{ \"a\": 1 }" "{\"a\":1}"
  , ok "{ \"a-b\": 1 }" "{\"a-b\":1}"
  , ok "{ 'a': 1 }" "{\"a\":1}"
  , ok "{ 1: x }" "{1:x}"
  , ok "{ 1.5: x }" "{1.5:x}"
  , ok "{ .5: x }" "{.5:x}"
  , ok "{ 0x1F: x }" "{0x1F:x}"
  , ok "{ 1n: x }" "{1n:x}"
  , ok "{ \"__proto__\": p }" "{\"__proto__\":p}"
  , ok "{ a: 1, \"b\": 2, 3: c, [d]: e }" "{a:1 \"b\":2 3:c [d]:e}"

  , bad "{ if }"
  , bad "{ a: }"
  , bad "{ a 1 }"
  , bad "{ a: 1"
  , bad "{ \"a\" }"
  , bad "{ 1 }"
  , bad "{ [k] }"
  , bad "{ \"a\" = 1 }"
  , bad "{ 1 = 1 }"
  , bad "{ [k]: }"
  , bad "{ \"a\": }"
  , bad "{ #a: 1 }"
  , bad "{ #a }"
  , bad "{ r\"x\": 1 }"
  ]

objectMethods :: [Test]
objectMethods =
  [ ok "{ fn f(x) => x }" "{(method f (x) x)}"
  , ok "{ async fn f() => 1 }" "{(async-method f () 1)}"
  , ok "{ fn [k]() => 1 }" "{(method [k] () 1)}"
  , ok "{ fn \"a-b\"() => 1 }" "{(method \"a-b\" () 1)}"
  , ok "{ fn 0() => 1 }" "{(method 0 () 1)}"
  , ok "{ fn if() => 1 }" "{(method if () 1)}"
  , ok "{ fn fn() => 1 }" "{(method fn () 1)}"
  , ok "{ fn async() => 1 }" "{(method async () 1)}"
  , ok "{ fn f(...xs) => xs }" "{(method f (...xs) xs)}"
  , ok "{ fn f({ a }) => a }" "{(method f ({a}) a)}"
  , ok "{ fn f() => do { a; b } }" "{(method f () (do a b))}"
  , ok "{ a: 1, fn f() => this.a }" "{a:1 (method f () (. this a))}"
  , ok "{ fn f() => 1, fn g() => 2, }" "{(method f () 1) (method g () 2)}"

  -- fn and async remain usable as plain keys
  , ok "{ fn: 1 }" "{fn:1}"
  , ok "{ async: 1 }" "{async:1}"
  , ok "{ async }" "{async}"
  , ok "{ async = 1 }" "{async=1}"

  , bad "{ fn #f() => 1 }"
  , bad "{ fn f() }"
  , bad "{ fn f() => }"
  , bad "{ fn f }"
  , bad "{ fn () => 1 }"
  , bad "{ async f() => 1 }"
  , bad "{ static fn f() => 1 }"
  , bad "{ fn f() => 1 g }"
  ]

-- null, true, false are valid property names in JS
literalNames :: [Test]
literalNames =
  [ ok "a.null" "(. a null)"
  , ok "a.true" "(. a true)"
  , ok "a?.false" "(?. a false)"
  , ok "{ null: 1, true: 2 }" "{null:1 true:2}"
  , ok "{ fn null() => 1 }" "{(method null () 1)}"
  , ok "class A where { fn true() => 1 }" "(class A {(method true () 1)})"
  , ok "import { null as n } from \"m\"" "(import {(as null n)} \"m\")"
  , ok "export { a as true }" "(export {(as a true)})"

  , bad "{ null }"
  , bad "{ true = 1 }"
  ]

postfix :: [Test]
postfix =
  [ ok "a.b" "(. a b)"
  , ok "a . b" "(. a b)"
  , ok "a.b.c" "(. (. a b) c)"
  , ok "a.if" "(. a if)"
  , ok "a[0]" "([] a 0)"
  , ok "a[b + 1]" "([] a (+ b 1))"
  , ok "f()" "(f)"
  , ok "f(a)" "(f a)"
  , ok "f(a, b)" "(f a b)"
  , ok "f()(b)" "((f) b)"
  , ok "a.b(c).d[0]" "([] (. ((. a b) c) d) 0)"

  , ok "a?.b" "(?. a b)"
  , ok "a?.(x)" "(?.() a x)"
  , ok "a?.[0]" "(?.[] a 0)"
  , ok "a?.b.c" "(. (?. a b) c)"
  , ok "a?.b?.c" "(?. (?. a b) c)"

  , bad "a."
  , bad "a?."
  , bad "a.1"
  , bad "a.5"
  , bad "a[]"
  , bad "f(a"
  ]

privateMembers :: [Test]
privateMembers =
  [ ok "this.#x" "(. this #x)"
  , ok "a?.#x" "(?. a #x)"
  , ok "this.#x.y" "(. (. this #x) y)"
  , ok "this.#f()" "((. this #f))"
  , ok "this.#xs[0]" "([] (. this #xs) 0)"
  , ok "a.#if" "(. a #if)"
  , ok "this.#x = 1" "(= (. this #x) 1)"
  , ok "this.#n += 1" "(+= (. this #n) 1)"
  , ok "{ a: this.#x }" "{a:(. this #x)}"
  , ok "a?.new" "(?. a new)"

  , bad "#x"
  , bad "a.#"
  , bad "a.#1"
  , bad "let #x = 1"
  , bad "fn(#x) => x"
  ]

spreadArgs :: [Test]
spreadArgs =
  [ ok "f(...xs)" "(f ...xs)"
  , ok "f(a, ...xs)" "(f a ...xs)"
  , ok "f(a, ...xs, b)" "(f a ...xs b)"
  , ok "f(...a, ...b)" "(f ...a ...b)"
  , ok "f(...a + b)" "(f ...(+ a b))"
  , ok "f(...xs.ys)" "(f ...(. xs ys))"
  , ok "f(...xs)(y)" "((f ...xs) y)"
  , ok "a?.(...xs)" "(?.() a ...xs)"
  , ok "new Foo(...args)" "(new Foo ...args)"
  , ok "new Foo(a, ...bs)" "(new Foo a ...bs)"

  , bad "f(...)"
  , bad "f(... , a)"
  , bad "f(xs...)"
  ]

-- `new` binds exactly as in JS
newTests :: [Test]
newTests =
  [ ok "new Foo()" "(new Foo)"
  , ok "new Foo" "(new Foo)"
  , ok "new Foo(x)" "(new Foo x)"
  , ok "new Foo(x, y)" "(new Foo x y)"
  , ok "new a.b.C(x)" "(new (. (. a b) C) x)"
  , ok "new Foo[0](x)" "(new ([] Foo 0) x)"
  , ok "new Foo.bar" "(new (. Foo bar))"
  , ok "new Foo.bar()" "(new (. Foo bar))"
  , ok "new (Foo)()" "(new Foo)"
  , ok "new (f())" "(new (f))"
  , ok "new (f())(x)" "(new (f) x)"
  , ok "new this.Foo()" "(new (. this Foo))"
  , ok "new import.meta.Foo()" "(new (. import.meta Foo))"

  -- postfix operations after the arguments apply to the new object
  , ok "new Foo().bar" "(. (new Foo) bar)"
  , ok "new Foo()(x)" "((new Foo) x)"
  , ok "new Foo()[0]" "([] (new Foo) 0)"
  , ok "new Foo()?.bar" "(?. (new Foo) bar)"
  , ok "new Date().getTime()" "((. (new Date) getTime))"
  , ok "new Foo(x).y(z)" "((. (new Foo x) y) z)"

  , ok "new new Foo()()" "(new (new Foo))"
  , ok "new new Foo" "(new (new Foo))"

  , ok "-new Foo()" "(- (new Foo))"
  , ok "await new Foo()" "(await (new Foo))"
  , ok "new Foo + 1" "(+ (new Foo) 1)"

  , ok "new.target" "new.target"
  , ok "new.target.name" "(. new.target name)"
  , ok "new.target()" "(new.target)"
  , ok "new new.target()" "(new new.target)"

  , bad "new"
  , bad "new Foo(x"
  , bad "new a?.b()"
  , bad "new a?.b"
  , bad "new -x"
  , bad "new typeof x"
  , bad "new await x"
  , bad "new import(\"m\")"
  , bad "new.foo"
  , bad "new."
  ]

unary :: [Test]
unary =
  [ ok (unop2str o <> " x") ("(" <> unop2str o <> " x)") | o <- [minBound .. maxBound] ]
  <>
  [ ok "-x" "(- x)"
  , ok "!x" "(! x)"
  , ok "- -x" "(- (- x))"
  , ok "+ +x" "(+ (+ x))"
  , ok "!!x" "(! (! x))"
  , ok "~x" "(~ x)"
  , ok "~~x" "(~ (~ x))"
  , ok "-~x" "(- (~ x))"
  , ok "~-x" "(~ (- x))"
  , ok "~a + b" "(+ (~ a) b)"
  , ok "typeof typeof x" "(typeof (typeof x))"
  , ok "void 0" "(void 0)"
  , ok "await f()" "(await (f))"
  , ok "-x.y" "(- (. x y))"
  , ok "-f(x)" "(- (f x))"

  , bad "-"
  , bad "~"
  , bad "typeof"
  ]

-- ++ and -- are not supported; the error suggests the alternative
incDec :: [Test]
incDec =
  [ badMsg "x++" "+= 1"
  , badMsg "++x" "+= 1"
  , badMsg "x--" "-= 1"
  , badMsg "--x" "-= 1"
  , badMsg "a.b++" "+= 1"
  , badMsg "f(x++)" "+= 1"
  , badMsg "[--x]" "-= 1"

  , bad "a++b"
  , bad "a--b"

  , ok "a - -b" "(- a (- b))"
  , ok "a + +b" "(+ a (+ b))"
  ]

binary :: [Test]
binary =
  [ ok ("a " <> s <> " b") ("(" <> s <> " a b)") | o <- [minBound .. maxBound], let s = binop2str o ]

precedence :: [Test]
precedence =
  [ ok "a + b * c" "(+ a (* b c))"
  , ok "a * b + c" "(+ (* a b) c)"
  , ok "a - b - c" "(- (- a b) c)"
  , ok "a - b + c" "(+ (- a b) c)"
  , ok "a / b / c" "(/ (/ a b) c)"
  , ok "a / b * c" "(* (/ a b) c)"
  , ok "a * b % c" "(% (* a b) c)"
  , ok "a % b + c" "(+ (% a b) c)"
  , ok "a ** b ** c" "(** a (** b c))"
  , ok "a * b ** c" "(* a (** b c))"
  , ok "a + b << c" "(<< (+ a b) c)"
  , ok "a << b + c" "(<< a (+ b c))"
  , ok "a >> b >>> c" "(>>> (>> a b) c)"
  , ok "a << b < c" "(< (<< a b) c)"
  , ok "a < b == c" "(== (< a b) c)"
  , ok "a + b instanceof c" "(instanceof (+ a b) c)"
  , ok "a instanceof b == c" "(== (instanceof a b) c)"
  , ok "a + b in c" "(in (+ a b) c)"
  , ok "a in b == c" "(== (in a b) c)"
  , ok "a == b & c" "(& (== a b) c)"
  , ok "a & b == c" "(& a (== b c))"
  , ok "a & b ^ c" "(^ (& a b) c)"
  , ok "a ^ b | c" "(| (^ a b) c)"
  , ok "a | b & c" "(| a (& b c))"
  , ok "a | b && c" "(&& (| a b) c)"
  , ok "a && b || c" "(|| (&& a b) c)"
  , ok "a && b && c" "(&& a (&& b c))"
  , ok "a || b || c" "(|| a (|| b c))"
  , ok "a ?? b ?? c" "(?? a (?? b c))"
  , ok "a < b == c < d" "(== (< a b) (< c d))"

  , ok "-a * b" "(* (- a) b)"
  , ok "-a + b" "(+ (- a) b)"
  , ok "a + -b" "(+ a (- b))"
  , ok "a ** -b" "(** a (- b))"
  , ok "!a && b" "(&& (! a) b)"
  , ok "!a == b" "(== (! a) b)"
  , ok "typeof -x" "(typeof (- x))"
  , ok "typeof a + b" "(+ (typeof a) b)"
  , ok "delete a && b" "(&& (delete a) b)"

  , ok "(a + b) * c" "(* (+ a b) c)"
  , ok "a * (b + c)" "(* a (+ b c))"
  , ok "((a))" "a"
  ]

-- JS forbids mixing ?? with || or &&, and a unary operand left of **
mixing :: [Test]
mixing =
  [ ok "(a || b) ?? c" "(?? (|| a b) c)"
  , ok "a ?? (b || c)" "(?? a (|| b c))"
  , ok "(a && b) ?? c" "(?? (&& a b) c)"
  , ok "a ?? (b && c)" "(?? a (&& b c))"
  , ok "a | b ?? c" "(?? (| a b) c)"
  , ok "a ?? b == c" "(?? a (== b c))"

  , ok "(-a) ** b" "(** (- a) b)"
  , ok "-(a ** b)" "(- (** a b))"
  , ok "(typeof a) ** b" "(** (typeof a) b)"
  , ok "a ** -b" "(** a (- b))"
  , ok "a ** (-b) ** c" "(** a (** (- b) c))"
  , ok "new Foo ** 2" "(** (new Foo) 2)"
  , ok "a.b ** 2" "(** (. a b) 2)"

  , bad "a ?? b || c"
  , bad "a || b ?? c"
  , bad "a && b ?? c"
  , bad "a ?? b && c"
  , bad "a ?? b || c ?? d"
  , bad "a || b && c ?? d"

  , bad "-a ** b"
  , bad "+a ** b"
  , bad "!a ** b"
  , bad "~a ** b"
  , bad "- -a ** b"
  , bad "typeof a ** b"
  , bad "void a ** b"
  , bad "delete a ** b"
  , bad "await a ** b"
  , bad "a ** -b ** c"
  , bad "a * -b ** c"
  ]

nonAssoc :: [Test]
nonAssoc =
  [ bad "a == b == c"
  , bad "a != b != c"
  , bad "a == b != c"
  , bad "a < b < c"
  , bad "a < b <= c"
  , bad "a > b >= c"
  , bad "a instanceof b instanceof c"
  , bad "a < b instanceof c"
  , bad "a in b in c"
  , bad "a < b in c"
  , bad "a in b instanceof c"
  ]

ins :: [Test]
ins =
  [ ok "\"x\" in o" "(in \"x\" o)"
  , ok "k in o.p" "(in k (. o p))"
  , ok "a in b && c" "(&& (in a b) c)"
  , ok "!(k in o)" "(! (in k o))"
  , ok "(k in o) == b" "(== (in k o) b)"
  , ok "#x in o" "(in #x o)"
  , ok "#x in this" "(in #x this)"
  , ok "fn(o) => #x in o" "(fn (o) (in #x o))"
  , ok "o.in" "(. o in)"
  , ok "{ in: 1 }" "{in:1}"

  , bad "#x.y in o"
  , bad "in o"
  , bad "a in"
  ]

deletes :: [Test]
deletes =
  [ ok "delete x.y" "(delete (. x y))"
  , ok "delete x[0]" "(delete ([] x 0))"
  , ok "delete x?.y" "(delete (?. x y))"
  , ok "!delete x.y" "(! (delete (. x y)))"
  , ok "delete a.b && c" "(&& (delete (. a b)) c)"

  , bad "delete"
  ]

lambdas :: [Test]
lambdas =
  [ ok "fn() => x" "(fn () x)"
  , ok "fn(x) => x" "(fn (x) x)"
  , ok "fn(x, y) => x" "(fn (x y) x)"
  , ok "fn(x) => fn(y) => x" "(fn (x) (fn (y) x))"
  , ok "fn(x) => x + 1" "(fn (x) (+ x 1))"
  , ok "fn(x) => x = 1" "(fn (x) (= x 1))"
  , ok "fn(x) => if a then b else c" "(fn (x) (if a b c))"
  , ok "fn(x) => do { a; b }" "(fn (x) (do a b))"
  , ok "(fn(x) => x)(1)" "((fn (x) x) 1)"
  , ok "f(fn(x) => x, y)" "(f (fn (x) x) y)"

  , bad "fn => x"
  , bad "fn(x"
  , bad "fn x => x"
  , bad "fn(x) x"
  , bad "fn(x) -> x"
  , bad "fn(x) =>"
  ]

params :: [Test]
params =
  [ ok "fn(...xs) => xs" "(fn (...xs) xs)"
  , ok "fn(a, ...xs) => a" "(fn (a ...xs) a)"
  , ok "fn(a, b, ...xs) => a" "(fn (a b ...xs) a)"
  , ok "fn(x = 1) => x" "(fn (x=1) x)"
  , ok "fn(x, y = 2) => y" "(fn (x y=2) y)"
  , ok "fn(x = a + b) => x" "(fn (x=(+ a b)) x)"
  , ok "fn(f = fn() => 1) => f" "(fn (f=(fn () 1)) f)"
  , ok "fn(x = 1, ...ys) => x" "(fn (x=1 ...ys) x)"
  , ok "fn(...xs,) => xs" "(fn (...xs) xs)"

  , bad "fn(...xs, a) => a"
  , bad "fn(a ...xs) => a"
  , bad "fn(...) => a"
  , bad "fn(...xs = 1) => xs"
  , bad "fn(x = ) => x"
  ]

asyncs :: [Test]
asyncs =
  [ ok "async fn() => x" "(async-fn () x)"
  , ok "async fn(x) => await f(x)" "(async-fn (x) (await (f x)))"
  , ok "const f = async fn() => 1" "(const f (async-fn () 1))"
  , ok "f(async fn() => 1)" "(f (async-fn () 1))"
  , ok "async" "async"
  , ok "async(x)" "(async x)"
  , ok "async + 1" "(+ async 1)"
  , ok "class A where { async fn f() => 1 }" "(class A {(async-method f () 1)})"
  , ok "class A where { static async fn f() => 1 }" "(class A {(static (async-method f () 1))})"
  , ok "class A where { async = 1 }" "(class A {(field async 1)})"

  , bad "async x"
  , bad "async fn"
  ]

trailingCommas :: [Test]
trailingCommas =
  [ ok "[1,]" "[1]"
  , ok "[1, 2,]" "[1 2]"
  , ok "[...xs,]" "[...xs]"
  , ok "{ a: 1, }" "{a:1}"
  , ok "{ a, }" "{a}"
  , ok "f(a,)" "(f a)"
  , ok "f(a, b,)" "(f a b)"
  , ok "f(...xs,)" "(f ...xs)"
  , ok "a?.(x,)" "(?.() a x)"
  , ok "new Foo(x,)" "(new Foo x)"
  , ok "fn(x,) => x" "(fn (x) x)"
  , ok "fn(x, y = 1,) => x" "(fn (x y=1) x)"

  , bad "[1,,]"
  , bad "{ a: 1,, }"
  , bad "f(,)"
  , bad "f(a,,)"
  , bad "fn(,) => x"
  , bad "fn(x,,) => x"
  ]

conditionals :: [Test]
conditionals =
  [ ok "if a then b else c" "(if a b c)"
  , ok "if a then b else if c then d else e" "(if a b (if c d e))"
  , ok "if a then if b then c else d else e" "(if a (if b c d) e)"
  , ok "if a == b then c else d" "(if (== a b) c d)"
  , ok "if a then b else c + d" "(if a b (+ c d))"
  , ok "(if a then b else c) + d" "(+ (if a b c) d)"
  , ok "if x = 1 then a else b" "(if (= x 1) a b)"

  , bad "if a then b"   -- divergence: else is mandatory
  , bad "if a b else c"
  , bad "if then a else b"
  , bad "if a then else b"
  ]

tries :: [Test]
tries =
  [ ok "try a catch e => b" "(try a (catch e b))"
  , ok "try a catch => b" "(try a (catch b))"
  , ok "try a finally c" "(try a (finally c))"
  , ok "try a catch e => b finally c" "(try a (catch e b) (finally c))"
  , ok "try a catch e => b + 1" "(try a (catch e (+ b 1)))"
  , ok "try f() catch e => throw e" "(try (f) (catch e (throw e)))"
  , ok "x = try a catch e => b" "(= x (try a (catch e b)))"
  , ok "const d = try JSON.parse(s) catch => null" "(const d (try ((. JSON parse) s) (catch null)))"
  , ok "try do { a; b } catch e => do { c }" "(try (do a b) (catch e (do c)))"
  , ok "p.catch(f)" "((. p catch) f)"
  , ok "p.finally(g)" "((. p finally) g)"
  , ok "class A where { fn catch(f) => f }" "(class A {(method catch (f) f)})"

  , bad "try a"
  , bad "try catch e => b"
  , bad "try a catch e b"
  , bad "try a finally"
  , bad "try a catch e => b catch f => c"
  , bad "try a finally b catch e => c"
  , bad "let try = 1"
  , bad "let catch = 1"
  , bad "let finally = 1"
  ]

throws :: [Test]
throws =
  [ ok "throw e" "(throw e)"
  , ok "throw new Error(\"x\")" "(throw (new Error \"x\"))"
  , ok "throw a ?? b" "(throw (?? a b))"
  , ok "fn() => throw e" "(fn () (throw e))"
  , ok "if c then x else throw e" "(if c x (throw e))"
  , ok "a ?? (throw e)" "(?? a (throw e))"
  , ok "do { throw e }" "(do (throw e))"
  , ok "x = throw e" "(= x (throw e))"

  , bad "throw"
  , bad "a ?? throw e"  -- divergence: throw takes everything to its right
  ]

assignment :: [Test]
assignment =
  [ ok ("x " <> s <> " y") ("(" <> s <> " x y)") | o <- [minBound .. maxBound], let s = assignop2str o ]
  <>
  [ ok "a.b = c" "(= (. a b) c)"
  , ok "a[0] = b" "(= ([] a 0) b)"
  , ok "a = b = c" "(= a (= b c))"
  , ok "a = b += 1" "(= a (+= b 1))"
  , ok "(a = b) = c" "(= (= a b) c)"
  , ok "x = a + b * c" "(= x (+ a (* b c)))"
  , ok "x = fn() => 1" "(= x (fn () 1))"
  , ok "x = if a then b else c" "(= x (if a b c))"
  , ok "x = a == b" "(= x (== a b))"
  , ok "x == 1" "(== x 1)"
  , ok "x >= 1" "(>= x 1)"
  , ok "(x = 1)" "(= x 1)"
  , ok "((((((((((((((((((((x))))))))))))))))))))" "x"
  , ok "[a, b] = [b, a]" "(= [a b] [b a])"
  -- permissive: target legality is the validator's job
  , ok "a + b = c" "(= (+ a b) c)"
  , ok "f() = 1" "(= (f) 1)"
  , ok "{ a, b } = o" "(= {a b} o)"
  , ok "({ a: x.y } = o)" "(= {a:(. x y)} o)"

  , bad "x ="
  , bad "= 1"
  , bad "x += "
  , bad "x = = 1"
  ]

declarations :: [Test]
declarations =
  [ ok "let x = 1" "(let x 1)"
  , ok "const x = 1" "(const x 1)"
  , ok "let x = a + b" "(let x (+ a b))"
  , ok "const f = fn(x) => x" "(const f (fn (x) x))"
  , ok "let x = if a then b else c" "(let x (if a b c))"
  , ok "let x = do { a; b }" "(let x (do a b))"
  , ok "const x = y = 1" "(const x (= y 1))"
  , ok "let x = y += 1" "(let x (+= y 1))"
  , ok "let [a, b] = xs" "(let [a b] xs)"
  , ok "let [a, ...rest] = xs" "(let [a ...rest] xs)"
  , ok "let [a, [b, c]] = xs" "(let [a [b c]] xs)"
  , ok "let [a = 1] = xs" "(let [(= a 1)] xs)"
  , ok "let {a, b} = o" "(let {a b} o)"
  , ok "let {a: x, ...o2} = o" "(let {a:x ...o2} o)"
  , ok "let {a: {b}} = o" "(let {a:{b}} o)"
  , ok "let {a = 1} = o" "(let {a=1} o)"

  , bad "let x"          -- initializer always required
  , bad "const x"
  , bad "let = 1"
  , bad "let x = "
  , bad "let x += 1"
  ]

patterns :: [Test]
patterns =
  [ bad "let [, b] = xs"  -- divergence: no holes
  , bad "let [a, , b] = xs"
  , bad "let [,] = xs"
  , bad "let [a,,] = xs"
  , bad "let [a b] = xs"

  , ok "let [a,] = xs" "(let [a] xs)"
  , ok "let [] = xs" "(let [] xs)"
  , ok "let {} = o" "(let {} o)"
  , ok "let [a, ...[b, c]] = xs" "(let [a ...[b c]] xs)"
  , ok "let [a, ...r,] = xs" "(let [a ...r] xs)"
  , ok "let { a, ...r, } = o" "(let {a ...r} o)"
  , ok "let { [k]: v } = o" "(let {[k]:v} o)"
  , ok "let { default: d } = m" "(let {default:d} m)"
  , ok "let { a: [b, { c }] } = o" "(let {a:[b {c}]} o)"
  , ok "let { [\"a-b\"]: x } = o" "(let {[\"a-b\"]:x} o)"
  , ok "let { [0]: x } = o" "(let {[0]:x} o)"
  , ok "let { \"a-b\": x } = o" "(let {\"a-b\":x} o)"
  , ok "let { 0: x } = o" "(let {0:x} o)"
  , ok "fn({ a, b }) => a" "(fn ({a b}) a)"
  , ok "fn([x, y]) => x" "(fn ([x y]) x)"
  , ok "fn([first, ...others]) => first" "(fn ([first ...others]) first)"
  , ok "fn(...[a, b]) => a" "(fn (...[a b]) a)"
  , ok "fn(x, { y }) => y" "(fn (x {y}) y)"
  , ok "fn({ a: { b } }) => b" "(fn ({a:{b}}) b)"
  , ok "class A where { fn f({ x, y }) => x }" "(class A {(method f ({x y}) x)})"
  , ok "try a catch { message } => message" "(try a (catch {message} message))"
  ]

-- strict-mode reserved words (static, implements, ...) parse as identifiers;
-- rejecting them as bindings is the validator's job
keywordBindings :: [Test]
keywordBindings =
  [ ok "let async = 1" "(let async 1)"
  , ok "let readonly = 1" "(let readonly 1)"
  , ok "let type = 1" "(let type 1)"
  , ok "let as = 1" "(let as 1)"
  , ok "abstract + 1" "(+ abstract 1)"
  , ok "abstract()" "(abstract)"
  , ok "abstract.x" "(. abstract x)"
  , ok "this" "this"
  , ok "this.x" "(. this x)"
  , ok "express.static(dir)" "((. express static) dir)"
  , ok "a.extends" "(. a extends)"
  , ok "m.delete(k)" "((. m delete) k)"
  , ok "o.default" "(. o default)"
  , ok "{ static: 1, class: 2 }" "{static:1 class:2}"
  , ok "{ readonly: true }" "{readonly:true}"

  , bad "let extends = 1"
  , bad "let class = 1"
  , bad "let delete = 1"
  , bad "let throw = 1"
  , bad "let in = 1"
  , bad "let import = 1"
  , bad "let export = 1"
  , bad "let fn = 1"
  ]

classes :: [Test]
classes =
  [ ok "class A where {}" "(class A {})"
  , ok "class A where { x }" "(class A {(field x)})"
  , ok "class A where { x = 1 }" "(class A {(field x 1)})"
  , ok "class A where { x = 1; y = 2 }" "(class A {(field x 1) (field y 2)})"
  , ok "class A where { x = 1; }" "(class A {(field x 1)})"
  , ok "class A where { #x = 1 }" "(class A {(field #x 1)})"
  , ok "class A where { onClick = fn(e) => e }" "(class A {(field onClick (fn (e) e))})"
  , ok "class A where { type = \"a\" }" "(class A {(field type \"a\")})"

  , ok "class A where { fn f() => 1 }" "(class A {(method f () 1)})"
  , ok "class A where { fn constructor(x) => this.x = x }" "(class A {(method constructor (x) (= (. this x) x))})"
  , ok "class A where { fn #helper() => 1 }" "(class A {(method #helper () 1)})"
  , ok "class A where { fn [k]() => 1 }" "(class A {(method [k] () 1)})"
  , ok "class A where { fn f(...xs) => xs }" "(class A {(method f (...xs) xs)})"
  , ok "class A where { fn delete(id) => id }" "(class A {(method delete (id) id)})"
  , ok "class A where { fn #check(o) => #x in o }" "(class A {(method #check (o) (in #x o))})"

  -- string and number keys
  , ok "class A where { \"a-b\" = 1 }" "(class A {(field \"a-b\" 1)})"
  , ok "class A where { 0 = 1 }" "(class A {(field 0 1)})"
  , ok "class A where { \"a\" }" "(class A {(field \"a\")})"
  , ok "class A where { fn \"a-b\"() => 1 }" "(class A {(method \"a-b\" () 1)})"
  , ok "class A where { fn 0() => 1 }" "(class A {(method 0 () 1)})"
  , ok "class A where { static \"a\" = 1 }" "(class A {(static (field \"a\" 1))})"
  , ok "class A where { static \"a\" }" "(class A {(static (field \"a\"))})"
  , ok "class A where { static 0 = 1 }" "(class A {(static (field 0 1))})"
  , ok "class A where { static fn \"a\"() => 1 }" "(class A {(static (method \"a\" () 1))})"

  , ok "class A where { static count = 0 }" "(class A {(static (field count 0))})"
  , ok "class A where { static #n = 0 }" "(class A {(static (field #n 0))})"
  , ok "class A where { static fn f() => 1 }" "(class A {(static (method f () 1))})"
  , ok "class A where { static readonly }" "(class A {(static (field readonly))})"

  , ok "class B extends A where {}" "(class B (extends A) {})"
  , ok "class B extends ns.A where {}" "(class B (extends (. ns A)) {})"
  , ok "class B extends mixin(A) where {}" "(class B (extends (mixin A)) {})"
  , ok "class where {}" "(class {})"
  , ok "class extends A where {}" "(class (extends A) {})"
  , ok "const A = class where { x = 1 }" "(const A (class {(field x 1)}))"

  , bad "class A"
  , bad "class A where { x y }"
  , bad "class A where { fn f }"
  , bad "class A where { fn f() }"      -- method body required
  , bad "class A where { fn f() => }"
  , bad "class A(1) where {}"
  , bad "class A extends where {}"
  , bad "class A where { x = }"
  , bad "class A where { x = 1"
  , bad "class A where { \"a\" \"b\" }"
  ]

reservedMembers :: [Test]
reservedMembers =
  [ ok "class A where { static = 1 }" "(class A {(field static 1)})"
  , ok "class A where { static }" "(class A {(field static)})"
  , ok "class A where { static static = 1 }" "(class A {(static (field static 1))})"
  , ok "class A where { abstract }" "(class A {(field abstract)})"
  , ok "class A where { fn = 1 }" "(class A {(field fn 1)})"
  , ok "class A where { fn static() => 1 }" "(class A {(method static () 1)})"
  , ok "class A where { static [\"readonly\"] = true }" "(class A {(static (field [\"readonly\"] true))})"
  , ok "class A where { static \"readonly\" = true }" "(class A {(static (field \"readonly\" true))})"
  ]

imports :: [Test]
imports =
  [ ok "import \"m\"" "(import \"m\")"
  , ok "import { a } from \"m\"" "(import {a} \"m\")"
  , ok "import { a, b as c } from \"m\"" "(import {a (as b c)} \"m\")"
  , ok "import { default as React } from \"react\"" "(import {(as default React)} \"react\")"
  , ok "import { delete as del } from \"m\"" "(import {(as delete del)} \"m\")"
  , ok "import { as } from \"m\"" "(import {as} \"m\")"
  , ok "import { as as x } from \"m\"" "(import {(as as x)} \"m\")"
  , ok "import * as ns from \"m\"" "(import *ns \"m\")"
  , ok "import {} from \"m\"" "(import {} \"m\")"
  , ok "import { a, } from \"m\"" "(import {a} \"m\")"
  , ok "import { \"a-b\" as x } from \"m\"" "(import {(as a-b x)} \"m\")"

  -- default imports
  , ok "import React from \"react\"" "(import React \"react\")"
  , ok "import React, { useState } from \"react\"" "(import React {useState} \"react\")"
  , ok "import React, { a, b as c, } from \"react\"" "(import React {a (as b c)} \"react\")"
  , ok "import React, {} from \"react\"" "(import React {} \"react\")"
  , ok "import React, * as R from \"react\"" "(import React *R \"react\")"
  , ok "import from from \"m\"" "(import from \"m\")"
  , ok "import as from \"m\"" "(import as \"m\")"

  -- parser accepts; the validator rejects `default` as a binding
  , ok "import { default } from \"m\"" "(import {default} \"m\")"
  , ok "import { a as default } from \"m\"" "(import {(as a default)} \"m\")"

  -- attributes
  , ok "import data from \"./d.json\" with { type: \"json\" }" "(import data \"./d.json\" (with {type:\"json\"}))"
  , ok "import \"m\" with { type: \"css\" }" "(import \"m\" (with {type:\"css\"}))"
  , ok "import { a } from \"m\" with { \"type\": \"json\" }" "(import {a} \"m\" (with {type:\"json\"}))"
  , ok "import * as ns from \"m\" with { type: \"json\", }" "(import *ns \"m\" (with {type:\"json\"}))"
  , ok "import x from \"m\" with { a: \"1\", b: \"2\" }" "(import x \"m\" (with {a:\"1\" b:\"2\"}))"
  , ok "import x from \"m\" with {}" "(import x \"m\")"

  -- dynamic import and import.meta
  , ok "import(\"m\")" "(import() \"m\")"
  , ok "import(\"m\",)" "(import() \"m\")"
  , ok "import(\"m\", { with: { type: \"json\" } })" "(import() \"m\" {with:{type:\"json\"}})"
  , ok "import(\"m\").then(f)" "((. (import() \"m\") then) f)"
  , ok "await import(\"m\")" "(await (import() \"m\"))"
  , ok "import.meta" "import.meta"
  , ok "import.meta.url" "(. import.meta url)"
  , ok "from" "from"
  , ok "let from = 1" "(let from 1)"
  , ok "with" "with"

  , bad "import { a } \"m\""
  , bad "import { a } from m"
  , bad "import * from \"m\""
  , bad "import * as ns"
  , bad "import"
  , bad "import.foo"
  , bad "import React from"
  , bad "import React \"m\""
  , bad "import React, \"m\""
  , bad "import React, from \"m\""
  , bad "import * as ns, React from \"m\""
  , bad "import { a }, React from \"m\""
  , bad "import React, { a }, * as ns from \"m\""
  , bad "import React, Other from \"m\""
  , bad "import \"m\" from \"n\""
  , bad "import { \"a-b\" } from \"m\""
  , bad "import { a as \"b\" } from \"m\""
  , bad "import x from \"m\" with { type: json }"
  , bad "import x from \"m\" with { type: 1 }"
  , bad "import x from \"m\" with type"
  , bad "import()"
  , bad "import(...xs)"
  , bad "import(a, ...xs)"
  , bad "import(a, b, c)"
  , bad "import(a,,)"
  ]

exports :: [Test]
exports =
  [ ok "export const x = 1" "(export (const x 1))"
  , ok "export let x = 1" "(export (let x 1))"
  , ok "export class A where {}" "(export (class A {}))"
  , ok "export const f = async fn() => 1" "(export (const f (async-fn () 1)))"
  , ok "export const { a, b } = o" "(export (const {a b} o))"
  , ok "export { a }" "(export {a})"
  , ok "export { a, b as c }" "(export {a (as b c)})"
  , ok "export { x as default }" "(export {(as x default)})"
  , ok "export { a, b as c } from \"m\"" "(export {a (as b c)} \"m\")"
  , ok "export { default as React } from \"react\"" "(export {(as default React)} \"react\")"
  , ok "export { default } from \"m\"" "(export {default} \"m\")"
  , ok "export * from \"m\"" "(export * \"m\")"
  , ok "export * as ns from \"m\"" "(export *ns \"m\")"

  -- export default
  , ok "export default x" "(export-default x)"
  , ok "export default 1 + 2" "(export-default (+ 1 2))"
  , ok "export default x = 1" "(export-default (= x 1))"
  , ok "export default fn() => 1" "(export-default (fn () 1))"
  , ok "export default async fn() => 1" "(export-default (async-fn () 1))"
  , ok "export default class A where {}" "(export-default (class A {}))"
  , ok "export default class where {}" "(export-default (class {}))"
  , ok "export default { a: 1 }" "(export-default {a:1})"
  , ok "export default if a then b else c" "(export-default (if a b c))"

  -- string names
  , ok "export { a as \"a-b\" }" "(export {(as a a-b)})"
  , ok "export { \"a-b\" } from \"m\"" "(export {a-b} \"m\")"
  , ok "export { \"a-b\" as c } from \"m\"" "(export {(as a-b c)} \"m\")"
  , ok "export * as \"a-b\" from \"m\"" "(export *a-b \"m\")"

  -- attributes
  , ok "export { a } from \"m\" with { type: \"json\" }" "(export {a} \"m\" (with {type:\"json\"}))"
  , ok "export * from \"m\" with { type: \"json\" }" "(export * \"m\" (with {type:\"json\"}))"
  , ok "export * as ns from \"m\" with { type: \"json\" }" "(export *ns \"m\" (with {type:\"json\"}))"

  , bad "export x"
  , bad "export 1"
  , bad "export fn f() => 1"
  , bad "export *"
  , bad "export * as ns"
  , bad "export"
  , bad "export default"
  , bad "export default let x = 1"
  , bad "export default const x = 1"
  , bad "export { \"a-b\" }"        -- a string local name needs `from`
  , bad "export { \"a-b\" as c }"
  , bad "export { a } with { type: \"json\" }"
  ]

blocks :: [Test]
blocks =
  [ ok "do {}" "(do)"
  , ok "do { a }" "(do a)"
  , ok "do { a; b }" "(do a b)"
  , ok "do { a; b; c }" "(do a b c)"
  , ok "do { a; }" "(do a)"
  , ok "do { let x = 1; x }" "(do (let x 1) x)"
  , ok "do { x = 1 }" "(do (= x 1))"
  , ok "do { do { a } }" "(do (do a))"
  , ok "where { a }" "(where a)"
  , ok "where { a; b }" "(where a b)"

  , bad "do { a b }"
  , bad "do a"
  , bad "do { a"
  , bad "do }"
  ]

loops :: [Test]
loops =
  [ ok "for x of xs do { f(x) }" "(for x xs (do (f x)))"
  , ok "for await c of s do { f(c) }" "(for-await c s (do (f c)))"
  , ok "for [k, v] of Object.entries(o) do { f(k, v) }" "(for [k v] ((. Object entries) o) (do (f k v)))"
  , ok "for { a } of xs do {}" "(for {a} xs (do))"
  , ok "for x of xs.filter(p) do { f(x) }" "(for x ((. xs filter) p) (do (f x)))"
  , ok "while x < 10 do { x += 1 }" "(while (< x 10) (do (+= x 1)))"
  , ok "while true do { break }" "(while true (do (break)))"
  , ok "for x of xs do { if p(x) then continue else f(x) }" "(for x xs (do (if (p x) (continue) (f x))))"

  , ok "return" "(return)"
  , ok "return x + 1" "(return (+ x 1))"
  , ok "if c then return else x" "(if c (return) x)"
  , ok "fn(x) => do { if x then return 1 else 2 }" "(fn (x) (do (if x (return 1) 2)))"
  , ok "do { return }" "(do (return))"
  , ok "a ?? (return)" "(?? a (return))"

  , ok "it.return(x)" "((. it return) x)"
  , ok "{ for: 1, while: 2 }" "{for:1 while:2}"
  , ok "of" "of"
  , ok "let of = 1" "(let of 1)"

  , bad "for x in xs do {}"  -- divergence: no for-in
  , bad "for (let i = 0; i < n; i += 1) do {}"
  , bad "for x of xs"
  , bad "for x of xs { f(x) }"
  , bad "for x of xs where { f(x) }"
  , bad "for x xs do {}"
  , bad "for if a then b else c of xs do {}"
  , bad "while x { f() }"
  , bad "while do {}"
  , bad "a ?? return"
  , bad "let for = 1"
  , bad "let while = 1"
  , bad "let return = 1"
  , bad "let break = 1"
  , bad "let continue = 1"
  ]

grouping :: [Test]
grouping =
  [ ok "(a)" "a"
  , ok "((a))" "a"
  , ok "(a + b)" "(+ a b)"

  , bad "()"
  , bad "(a"
  , bad "(a, b)" -- divergence: no comma operator
  , bad "(a,)"
  ]

-- TS-only syntax removed in the JS pivot
tsGone :: [Test]
tsGone = fmap bad
  [ "x as T"
  , "x satisfies T"
  , "x as const"
  , "x!"
  , "x!.y"
  , "fn(x: number) => x"
  , "fn(x): number => x"
  , "fn(x?) => x"
  , "forall T. fn(x: T) => x"
  , "let x: number = 1"
  , "let { a }: O = o"
  , "type Id = number"
  , "interface A where {}"
  , "abstract class A where {}"
  , "class A implements I where {}"
  , "class Box(T) where {}"
  , "class A where { x: number }"
  , "class A where { x?: number }"
  , "class A where { fn f(): number }"
  , "class A where { protected fn f() => 1 }"
  , "import { type T } from \"m\""
  , "import type { T } from \"m\""
  , "export type T = A"
  , "export { type T }"
  ]

topLevel :: [Test]
topLevel =
  [ top "" []
  , top "a" ["a"]
  , top "a\nb\nc" ["a", "b", "c"]
  , top "a;b" ["a", "b"]
  , top "a;" ["a"]
  , top "let x = 1" ["(let x 1)"]
  , top "if a then b else c" ["(if a b c)"]

  , top "do\n  a\n  b" ["(do a b)"]
  , top "where\n  a\n  b" ["(where a b)"]
  , top "do\n  a;b" ["(do a b)"]
  , top "do\n  a\n  do\n    b" ["(do a (do b))"]
  , top "do\n  a\n  do\n    b\n  c" ["(do a (do b) c)"]
  , top "do\n  a\nb" ["(do a)", "b"]
  , top "x\ndo\ny" ["x", "(do)", "y"]
  , top "do { a; b }" ["(do a b)"]
  , top "do\n  x += 1\n  x" ["(do (+= x 1) x)"]
  , top "x = do\n  a\n  b" ["(= x (do a b))"]

  , top "let x =\n  1" ["(let x 1)"]
  , top "a\n  + b" ["(+ a b)"]
  , top "if a\n  then b\n  else c" ["(if a b c)"]
  , top "if a\nthen b\nelse c" ["(if a b c)"]
  , top "do\n  if a\n  then b\n  else c" ["(do (if a b c))"]

  , top "let xs = [\n  1,\n  2\n]" ["(let xs [1 2])"]
  , top "f(\n  a,\n  b\n)" ["(f a b)"]
  , top "{\n  a: 1,\n  b: 2\n}" ["{a:1 b:2}"]
  , top "let xs = [\n  1,\n  2,\n]" ["(let xs [1 2])"]
  , top "f(\n  a,\n  b,\n)" ["(f a b)"]
  , top "{\n  a: 1,\n  b: 2,\n}" ["{a:1 b:2}"]
  , top "const f = fn(\n  a,\n  ...rest\n) => a" ["(const f (fn (a ...rest) a))"]
  , top "const Card = fn({\n  title,\n  body = \"\",\n}) => body" ["(const Card (fn ({title body=\"\"}) body))"]

  , top "let p = { x: 1, y: 2 }\nlet dist = fn(a) => do\n  let dx = a.x\n  dx * dx"
      [ "(let p {x:1 y:2})"
      , "(let dist (fn (a) (do (let dx (. a x)) (* dx dx))))"
      ]
  , top "const load = async fn(url) => do\n  const res = await fetch(url)\n  if res.ok then res.json() else throw new Error(\"failed\")"
      [ "(const load (async-fn (url) (do (const res (await (fetch url))) (if (. res ok) ((. res json)) (throw (new Error \"failed\"))))))" ]

  , top "const o = {\n  a: 1,\n  fn f() => this.a,\n  fn g() => do\n    log(1)\n    2\n}"
      [ "(const o {a:1 (method f () (. this a)) (method g () (do (log 1) 2))})" ]
  , top "const t = new Date()\n  .getTime()" ["(const t ((. (new Date) getTime)))"]

  , top "class A where\n  x = 1\n  fn f() => x" ["(class A {(field x 1) (method f () x)})"]
  , top "class B extends A where\n  fn constructor() => do\n    super()\n    this.x = 1\n  fn g() => 2"
      [ "(class B (extends A) {(method constructor () (do (super) (= (. this x) 1))) (method g () 2)})" ]
  , top "class A where\n  static count = 0\n\n  #items = []\n  fn push(x) => this.#items.push(x)"
      [ "(class A {(static (field count 0)) (field #items []) (method push (x) ((. (. this #items) push) x))})" ]
  , top "class E where\nx" ["(class E {})", "x"]

  , top "import {\n  a,\n  b,\n} from \"m\"\nimport * as ns from \"n\"\n\nexport const f = fn(x) => ns.g(a, x)"
      [ "(import {a b} \"m\")"
      , "(import *ns \"n\")"
      , "(export (const f (fn (x) ((. ns g) a x))))"
      ]
  , top "import React, {\n  useState,\n} from \"react\"\nconst App = fn() => 1\nexport default App"
      [ "(import React {useState} \"react\")"
      , "(const App (fn () 1))"
      , "(export-default App)"
      ]
  , top "export class A where\n  x = 1\n  fn f() => x" ["(export (class A {(field x 1) (method f () x)}))"]
  , top "const App = fn() => 1\nexport { App as default }" ["(const App (fn () 1))", "(export {(as App default)})"]

  , top "const data = try do\n    JSON.parse(s)\n  catch e => null"
      [ "(const data (try (do ((. JSON parse) s)) (catch e null)))" ]
  , top "do\n  try\n    risky()\n  catch e => log(e)\n  finally done()\n  next()"
      [ "(do (try (risky) (catch e (log e)) (finally (done))) (next))" ]

  , top "for x of xs do\n  f(x)\n  g(x)" ["(for x xs (do (f x) (g x)))"]
  , top "const sum = fn(xs) => do\n  let t = 0\n  for x of xs do\n    t += x\n  t"
      [ "(const sum (fn (xs) (do (let t 0) (for x xs (do (+= t x))) t)))" ]
  , top "while running do\n  step()\n  if done() then break else tick()"
      [ "(while running (do (step) (if (done) (break) (tick))))" ]
  , top "const f = fn(x) => do\n  if x then return else log(x)\n  x"
      [ "(const f (fn (x) (do (if x (return) (log x)) x)))" ]
  ]

-- divergence: an implicit block runs to the end of its last line,
-- so whatever follows it goes on a new, less-indented line, or the block uses braces
blockEnds :: [Test]
blockEnds =
  [ top "f(do\n  a\n)" ["(f (do a))"]
  , top "f(do { a })" ["(f (do a))"]
  , top "map(fn(x) => do { a }, xs)" ["(map (fn (x) (do a)) xs)"]
  , top "setTimeout(fn() => do\n    a\n  , 100)" ["(setTimeout (fn () (do a)) 100)"]
  , top "if c then do { a } else b" ["(if c (do a) b)"]
  , top "if c then do\n    a\n  else b" ["(if c (do a) b)"]
  , top "try do\n    a\n  catch e => b" ["(try (do a) (catch e b))"]
  , top "try do\n    a\n  finally b" ["(try (do a) (finally b))"]
  , top "do if a then b else c" ["(do (if a b c))"]
  , top "do try a catch e => b" ["(do (try a (catch e b)))"]

  , badTop "f(do a)"
  , badTop "[do a]"
  , badTop "{ k: do a }"
  , badTop "map(fn(x) => do a, xs)"
  , badTop "if c then do a else b"
  , badTop "try do a catch e => b"
  , badTop "try do a finally b"
  ]

topFailures :: [Test]
topFailures =
  [ badTop "a\n  b"
  , badTop "do\n  a\n b"
  , badTop "let x =\n1"
  , badTop "f(do\n  a\n  b)"
  , badTop "(a"
  , badTop "a)"
  , badTop "[1, 2"
  , badTop "class A where\n  x = 1\n y"
  , badTop "import { a }\n  from \"m\"\nx y"
  , badTop "x\n++"
  ]

spans :: [Test]
spans =
  [ spanOk "x" (0, 1)
  , spanOk "  x" (2, 3)
  , spanOk "\"a\\nb\"" (0, 6)
  , spanOk "a + b" (0, 5)
  , spanOk "a + b * c" (0, 9)
  , spanOk "a ?? b ?? c" (0, 11)
  , spanOk "a ** b" (0, 6)
  , spanOk "-x" (0, 2)
  , spanOk "a.b.c" (0, 5)
  , spanOk "f(x)" (0, 4)
  , spanOk "a?.[0]" (0, 6)
  , spanOk "new Foo(x).y" (0, 12)
  , spanOk "(a)" (0, 3)
  , spanOk "(a).b" (0, 5)
  , spanOk "((a))" (0, 5)
  , spanOk "(a + b) * c" (0, 11)
  , spanOk "x = y + 1" (0, 9)
  , spanOk "if a then b else c" (0, 18)
  , spanOk "fn(x) => x + 1" (0, 14)
  , spanOk "do { a; b }" (0, 11)
  , spanOk "let x = 1" (0, 9)
  , spanOk "class A where {}" (0, 16)
  , spanOk "import { a } from \"m\"" (0, 21)
  , spanOk "return" (0, 6)

  -- layout tokens never extend a span
  , topSpans "do\n  a\nb" [(0, 6), (7, 8)]
  , topSpans "x = do\n  a\n  b" [(0, 14)]
  , topSpans "x\ndo\ny" [(0, 1), (2, 4), (5, 6)]
  , topSpans "f(\n  a,\n  b\n)" [(0, 13)]
  , topSpans "class A where\n  x = 1\ny" [(0, 21), (22, 23)]

  -- columns count a tab as 8, offsets count it as 1
  , startOk "\tx" (1, 9)
  , spanOk "\tx" (1, 2)
  ]
