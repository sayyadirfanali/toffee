module TypeTests (tests) where

import Test.HUnit
import Text.Megaparsec (bundleErrors, parseErrorTextPretty)

import Lexer (runLex, Lit(..))
import Layout (layout)
import Parser
import Expr

typParse :: String -> IO (Either String TExpr)
typParse s = case runLex s of
  Left e   -> return (Left ("lex: " <> show e))
  Right ts -> case layout ts of
    Left e    -> return (Left ("layout: " <> show e))
    Right ys -> do
      r <- runParse (bracketP '<' *> texprE <* bracketP '>' <* eofP) ys
      return $ case r of
        Left e  -> Left ("parse: " <> concatMap parseErrorTextPretty (bundleErrors e))
        Right t -> Right t

typTest :: String -> TExpr -> Test
typTest inp exp = TestCase $ do
  res <- typParse inp
  case res of
    Left err -> assertFailure ("failed on " <> show inp <> ": " <> err)
    Right out -> out @?= exp

typFail :: String -> Test
typFail inp = TestCase $ do
  res <- typParse inp
  case res of
    Left _    -> return ()
    Right out -> assertFailure ("expected failure on " <> show inp <> ", got " <> show out)

ref n = TEApp [Ident n] []
app n = TEApp [Ident n]
un = TEBinOp TUnion
bi = TEBinOp TInter
tup = TETuple . fmap TTEPlain
key = PKMem . MNPub
fld k = TOFKeyVal (key k) False
opt k = TOFKeyVal (key k) True
prm  x = TFPIdent (Ident x) False
oprm x = TFPIdent (Ident x) True
rst  x = TFPRest (Ident x)
fun = TEFunc []
a = ref "A"
b = ref "B"
c = ref "C"
t = ref "T"

tests :: Test
tests = TestList $ concat
  [ atoms
  , application
  , operators
  , arrays
  , tuples
  , objects
  , objectKeys
  , functions
  , restParams
  , foralls
  , voids
  , raws
  , grouping
  , layoutTests
  , realistic
  ]

atoms :: [Test]
atoms =
  [ typTest n (ref n) | n <- words "string number boolean bigint symbol object unknown never any undefined this" ]
  <>
  [ typTest "Foo" (ref "Foo")
  , typTest "ns.Foo" (TEApp [Ident "ns", Ident "Foo"] [])
  , typTest "ns.Foo.Bar" (TEApp [Ident "ns", Ident "Foo", Ident "Bar"] [])

  , typTest "\"a\"" (TELit (LStr "a"))
  , typTest "'a'" (TELit (LStr "a"))
  , typTest "42" (TELit (LNum "42"))
  , typTest "1.5" (TELit (LNum "1.5"))
  , typTest "true" (TELit (LBool "true"))
  , typTest "false" (TELit (LBool "false"))
  , typTest "null" (TELit (LNull "null"))

  , typFail ""
  , typFail "ns."
  , typFail "ns.1"
  , typFail "-1" -- not supported yet
  , typFail "T[K]" -- not supported yet
  ]

application :: [Test]
application =
  [ typTest "Promise(T)" (app "Promise" [t])
  , typTest "Map(K, V)" (app "Map" [ref "K", ref "V"])
  , typTest "Promise(Array(T))" (app "Promise" [app "Array" [t]])
  , typTest "ns.Box(T)" (TEApp [Ident "ns", Ident "Box"] [t])
  , typTest "Foo()" (ref "Foo")

  , typTest "Box(A | B)" (app "Box" [un a b])
  , typTest "Box(A[])" (app "Box" [TEArr a])
  , typTest "Box([A, B])" (app "Box" [tup [a, b]])
  , typTest "Box({ a: A })" (app "Box" [TEObj [fld "a" a]])
  , typTest "Box(fn() => A)" (app "Box" [fun [] a])
  , typTest "Map(string, A[])" (app "Map" [ref "string", TEArr a])

  , typFail "Box("
  , typFail "Box(A,)"
  , typFail "Box A"
  , typFail "Foo<T>"
  ]

operators :: [Test]
operators =
  [ typTest "A | B" (un a b)
  , typTest "A & B" (bi a b)

  , typTest "A | B | C" (un (un a b) c)
  , typTest "A & B & C" (bi (bi a b) c)

  , typTest "A & B | C" (un (bi a b) c)
  , typTest "A | B & C" (un a (bi b c))
  , typTest "A | B & C | D" (un (un a (bi b c)) (ref "D"))
  , typTest "(A | B) & C" (bi (un a b) c)
  , typTest "A & (B | C)" (bi a (un b c))

  , typTest "\"a\" | \"b\"" (un (TELit (LStr "a")) (TELit (LStr "b")))
  , typTest "true | false" (un (TELit (LBool "true")) (TELit (LBool "false")))
  , typTest "A | null" (un a (TELit (LNull "null")))

  , typFail "A |"
  , typFail "A &"
  , typFail "| A" -- not supported yet
  , typFail "A | | B"
  ]

arrays :: [Test]
arrays =
  [ typTest "T[]" (TEArr t)
  , typTest "T[][]" (TEArr (TEArr t))
  , typTest "Array(T)[]" (TEArr (app "Array" [t]))
  , typTest "ns.Foo[]" (TEArr (TEApp [Ident "ns", Ident "Foo"] []))

  , typTest "A | B[]" (un a (TEArr b))
  , typTest "A & B[]" (bi a (TEArr b))
  , typTest "A & B[] | C" (un (bi a (TEArr b)) c)
  , typTest "(A | B)[]" (TEArr (un a b))
  , typTest "(A & B)[]" (TEArr (bi a b))
  , typTest "(A | B)[][]" (TEArr (TEArr (un a b)))

  , typFail "[]T"
  , typFail "T["
  ]

tuples :: [Test]
tuples =
  [ typTest "[]" (tup [])
  , typTest "[A]" (tup [a])
  , typTest "[A, B]" (tup [a, b])
  , typTest "[A, B, C]" (tup [a, b, c])
  , typTest "[[A], B]" (tup [tup [a], b])
  , typTest "[A, B][]" (TEArr (tup [a, b]))
  , typTest "[A | B, C]" (tup [un a b, c])
  , typTest "[A[], B]" (tup [TEArr a, b])
  , typTest "[fn() => A, B]" (tup [fun [] a, b])
  , typTest "[string, number] & { length: 2 }"
      (bi (tup [ref "string", ref "number"]) (TEObj [fld "length" (TELit (LNum "2"))]))

  , typFail "[A, B"
  , typFail "[A,]"
  , typFail "[,]"
  ]

objects :: [Test]
objects =
  [ typTest "{}" (TEObj [])
  , typTest "{ a: A }" (TEObj [fld "a" a])
  , typTest "{ a?: A }" (TEObj [opt "a" a])
  , typTest "{ a: A, b?: B }" (TEObj [fld "a" a, opt "b" b])
  , typTest "{ a ?: A }" (TEObj [opt "a" a])
  , typTest "{ a: A | B }" (TEObj [fld "a" (un a b)])
  , typTest "{ a: A[] }" (TEObj [fld "a" (TEArr a)])
  , typTest "{ a: [A, B] }" (TEObj [fld "a" (tup [a, b])])
  , typTest "{ a: { b: B } }" (TEObj [fld "a" (TEObj [fld "b" b])])
  , typTest "{ a: { b: { c: C } } }" (TEObj [fld "a" (TEObj [fld "b" (TEObj [fld "c" c])])])

  , typTest "{ f: fn() => A, b: B }" (TEObj [fld "f" (fun [] a), fld "b" b])

  , typTest "{ type: \"click\" }" (TEObj [fld "type" (TELit (LStr "click"))])
  , typTest "{ if: A, new: B }" (TEObj [fld "if" a, fld "new" b])

  , typFail "{ a }"
  , typFail "{ a: }"
  , typFail "{ a?: }"
  , typFail "{ a: A, }"
  , typFail "{ a: A"
  ]

objectKeys :: [Test]
objectKeys =
  [ typTest "{ \"a-b\": A }" (TEObj [TOFKeyVal (PKStr "a-b") False a])
  , typTest "{ \"a-b\"?: A }" (TEObj [TOFKeyVal (PKStr "a-b") True a])
  , typTest "{ 'x': A }" (TEObj [TOFKeyVal (PKStr "x") False a])
  , typTest "{ 0: A, 1: B }" (TEObj [TOFKeyVal (PKNum "0") False a, TOFKeyVal (PKNum "1") False b])
  , typTest "{ [k]: A }" (TEObj [TOFKeyVal (PKComp (EIden (Ident "k"))) False a])
  , typTest "{ [Symbol.iterator]: A }"
      (TEObj [TOFKeyVal (PKComp (EMem (EIden (Ident "Symbol")) (MNPub "iterator"))) False a])
  , typTest "{ a: A, \"b\": B, 2: C }"
      (TEObj [fld "a" a, TOFKeyVal (PKStr "b") False b, TOFKeyVal (PKNum "2") False c])

  , typFail "{ #a: A }"
  , typFail "{ \"a\" }"
  , typFail "{ [k] }"
  , typFail "{ [k]: }"
  ]

functions :: [Test]
functions =
  [ typTest "fn() => A" (fun [] a)
  , typTest "fn(x: A) => B" (fun [prm "x" a] b)
  , typTest "fn(x?: A) => B" (fun [oprm "x" a] b)
  , typTest "fn(x: A, y?: B) => C" (fun [prm "x" a, oprm "y" b] c)
  , typTest "fn(x: A[]) => B" (fun [prm "x" (TEArr a)] b)
  , typTest "fn(this: Window) => void" (fun [prm "this" (ref "Window")] (ref "void"))

  , typTest "fn(x: A) => B | C" (fun [prm "x" a] (un b c))
  , typTest "fn() => A[]" (fun [] (TEArr a))
  , typTest "fn() => (A | B)[]" (fun [] (TEArr (un a b)))
  , typTest "fn() => [A, B]" (fun [] (tup [a, b]))
  , typTest "fn() => { a: A }" (fun [] (TEObj [fld "a" a]))
  , typTest "fn(x: A) => fn(y: B) => C" (fun [prm "x" a] (fun [prm "y" b] c))

  , typTest "fn(f: fn(x: A) => B) => C" (fun [prm "f" (fun [prm "x" a] b)] c)
  , typTest "fn(f: fn() => A, y: B) => C" (fun [prm "f" (fun [] a), prm "y" b] c)

  , typTest "(fn(x: A) => B) | C" (un (fun [prm "x" a] b) c)
  , typTest "(fn() => A) & B" (bi (fun [] a) b)
  , typTest "(fn() => A)[]" (TEArr (fun [] a))
  , typTest "fn(x: A,) => B" (fun [prm "x" a] b)

  , typFail "A | fn() => B"
  , typFail "A & fn() => B"
  , typFail "fn(x) => A"
  , typFail "fn(x: A)"
  , typFail "fn(x: A) =>"
  , typFail "fn x: A => B"
  , typFail "fn(x: A) -> B"
  , typFail "fn(1: A) => B"
  , typFail "fn(x: A,,) => B"
  ]

restParams :: [Test]
restParams =
  [ typTest "fn(...xs: A[]) => B" (fun [rst "xs" (TEArr a)] b)
  , typTest "fn(x: A, ...ys: B[]) => C" (fun [prm "x" a, rst "ys" (TEArr b)] c)
  , typTest "fn(x?: A, ...ys: B[]) => C" (fun [oprm "x" a, rst "ys" (TEArr b)] c)
  , typTest "fn(...args: [A, B]) => C" (fun [rst "args" (tup [a, b])] c)
  , typTest "forall T. fn(...xs: T[]) => T" (TEFunc [Ident "T"] [rst "xs" (TEArr t)] t)
  , typTest "fn(f: fn(...xs: A[]) => B) => C" (fun [prm "f" (fun [rst "xs" (TEArr a)] b)] c)

  , typFail "fn(...xs) => A"          -- rest params in types need an annotation
  , typFail "fn(...xs: A[], y: B) => C"
  , typFail "fn(...xs: A[],) => B"
  , typFail "fn(x: A ...ys: B[]) => C"
  , typFail "fn(...: A[]) => B"
  , typFail "fn(...xs?: A[]) => B"
  ]

foralls :: [Test]
foralls =
  [ typTest "forall T. fn(x: T) => T" (TEFunc [Ident "T"] [prm "x" t] t)
  , typTest "forall T.fn(x: T) => T" (TEFunc [Ident "T"] [prm "x" t] t)
  , typTest "forall T U. fn(x: T) => U" (TEFunc [Ident "T", Ident "U"] [prm "x" t] (ref "U"))
  , typTest "forall K V. fn(m: Map(K, V), k: K) => V | undefined"
      (TEFunc [Ident "K", Ident "V"]
        [prm "m" (app "Map" [ref "K", ref "V"]), prm "k" (ref "K")]
        (un (ref "V") (ref "undefined")))

  , typTest "forall T. fn(x: T) => T | null" (TEFunc [Ident "T"] [prm "x" t] (un t (TELit (LNull "null"))))

  , typTest "fn(f: forall T. fn(x: T) => T) => number" (fun [prm "f" (TEFunc [Ident "T"] [prm "x" t] t)] (ref "number"))
  , typTest "Box(forall T. fn(x: T) => T)" (app "Box" [TEFunc [Ident "T"] [prm "x" t] t])
  , typTest "[forall T. fn(x: T) => T]" (tup [TEFunc [Ident "T"] [prm "x" t] t])
  , typTest "{ id: forall T. fn(x: T) => T }" (TEObj [fld "id" (TEFunc [Ident "T"] [prm "x" t] t)])

  , typTest "(forall T. fn(x: T) => T)[]" (TEArr (TEFunc [Ident "T"] [prm "x" t] t))
  , typTest "(forall T. fn() => T) | A" (un (TEFunc [Ident "T"] [] t) a)

  , typFail "forall. fn() => A"
  , typFail "forall T fn(x: T) => T"
  , typFail "forall T. A"
  , typFail "forall T."
  , typFail "forall 1. fn() => A"
  , typFail "forall T. forall U. fn() => U"
  , typFail "A | forall T. fn() => T"
  ]

voids :: [Test]
voids =
  [ typTest "void" (ref "void")
  , typTest "void[]" (TEArr (ref "void"))
  , typTest "void | undefined" (un (ref "void") (ref "undefined"))
  , typTest "Promise(void)" (app "Promise" [ref "void"])
  , typTest "fn(x: A) => void" (fun [prm "x" a] (ref "void"))
  ]

raws :: [Test]
raws =
  [ typTest "raw \"Partial<User>\"" (TERaw (Raw "Partial<User>"))
  , typTest "raw 'keyof T'" (TERaw (Raw "keyof T"))
  , typTest "raw \"`a${B}`\"" (TERaw (Raw "`a${B}`"))

  , typTest "raw \"A | B\"[]" (TEArr (TERaw (Raw "A | B")))
  , typTest "raw \"X\"[][]" (TEArr (TEArr (TERaw (Raw "X"))))
  , typTest "raw \"X\" | null" (un (TERaw (Raw "X")) (TELit (LNull "null")))
  , typTest "A & raw \"X\"" (bi a (TERaw (Raw "X")))
  , typTest "Promise(raw \"keyof T\")" (app "Promise" [TERaw (Raw "keyof T")])
  , typTest "fn() => raw \"X\"" (fun [] (TERaw (Raw "X")))

  , typFail "raw"
  , typFail "raw Partial"
  , typFail "raw 1"
  ]

grouping :: [Test]
grouping =
  [ typTest "(A)" a
  , typTest "((A))" a
  , typTest "(A | B)" (un a b)

  , typFail "()"
  , typFail "(A"
  , typFail "A)"
  , typFail "(A, B)"
  ]

layoutTests :: [Test]
layoutTests =
  [ typTest "A\n  | B\n  | C" (un (un a b) c)
  , typTest "A\n  & B" (bi a b)
  , typTest "{\n  a: A,\n  b: B\n}" (TEObj [fld "a" a, fld "b" b])
  , typTest "[\n  A,\n  B\n]" (tup [a, b])
  , typTest "Map(\n  K,\n  V\n)" (app "Map" [ref "K", ref "V"])
  , typTest "fn(\n  x: A,\n  y: B\n) => C" (fun [prm "x" a, prm "y" b] c)
  , typTest "fn(\n  x: A,\n  ...ys: B[]\n) => C" (fun [prm "x" a, rst "ys" (TEArr b)] c)
  , typTest "fn(x: A)\n  => B" (fun [prm "x" a] b)
  , typTest "forall K V.\n  fn(m: Map(K, V)) => K[]"
      (TEFunc [Ident "K", Ident "V"] [prm "m" (app "Map" [ref "K", ref "V"])] (TEArr (ref "K")))

  , typFail "A\n| B"
  , typFail "A |\nB"
  , typFail "fn(x: A)\n=> B"
  ]

realistic :: [Test]
realistic =
  [ typTest "Promise(Array(User))[] | null"
      (un (TEArr (app "Promise" [app "Array" [ref "User"]])) (TELit (LNull "null")))

  , typTest "{ id: number, tags?: string[], owner: { name: string } }"
      (TEObj [ fld "id" (ref "number")
             , opt "tags" (TEArr (ref "string"))
             , fld "owner" (TEObj [fld "name" (ref "string")]) ])

  , typTest "fn(req: Request, next?: fn() => undefined) => Promise(Response)"
      (fun [prm "req" (ref "Request"), oprm "next" (fun [] (ref "undefined"))]
           (app "Promise" [ref "Response"]))

  , typTest "{\n  onClick: fn(e: MouseEvent) => boolean,\n  items: (Item | Group)[]\n}"
      (TEObj [ fld "onClick" (fun [prm "e" (ref "MouseEvent")] (ref "boolean"))
             , fld "items" (TEArr (un (ref "Item") (ref "Group"))) ])

  , typTest "{ \"Content-Type\": string, \"X-Retry\"?: number }"
      (TEObj [ TOFKeyVal (PKStr "Content-Type") False (ref "string")
             , TOFKeyVal (PKStr "X-Retry") True (ref "number") ])

  , typTest "forall T. fn(...fns: (fn(x: T) => T)[]) => fn(x: T) => T"
      (TEFunc [Ident "T"]
        [rst "fns" (TEArr (fun [prm "x" t] t))]
        (fun [prm "x" t] t))
  ]