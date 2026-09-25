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
fld k = TOFKeyVal (Ident k) False
opt k = TOFKeyVal (Ident k) True
prm  x = TFPIdTyp (Ident x) False
oprm x = TFPIdTyp (Ident x) True
fun = TEFunc []
a = ref "A"
b = ref "B"
c = ref "C"
t = ref "T"

tests :: Test
tests = TestList
  [
    typTest "number" (ref "number")
  , typTest "Foo" (ref "Foo")
  , typTest "undefined" (ref "undefined")
  , typTest "ns.Foo" (TEApp [Ident "ns", Ident "Foo"] [])
  , typTest "\"a\"" (TELit (LStr "a"))
  , typTest "42" (TELit (LNum "42"))
  , typTest "true" (TELit (LBool "true"))
  , typTest "null" (TELit (LNull "null"))

  , typTest "Promise(T)" (app "Promise" [t])
  , typTest "Map(K, V)" (app "Map" [ref "K", ref "V"])
  , typTest "Promise(Array(T))" (app "Promise" [app "Array" [t]])
  , typTest "ns.Box(T)" (TEApp [Ident "ns", Ident "Box"] [t])

  , typTest "A | B" (un a b)
  , typTest "A | B | C" (un (un a b) c)
  , typTest "A & B & C" (bi (bi a b) c)
  , typTest "A & B | C" (un (bi a b) c)
  , typTest "A | B & C" (un a (bi b c))
  , typTest "(A | B) & C" (bi (un a b) c)
  , typTest "((A))" a

  , typTest "T[]" (TEArr t)
  , typTest "T[][]" (TEArr (TEArr t))
  , typTest "A | B[]" (un a (TEArr b))
  , typTest "(A | B)[]" (TEArr (un a b))
  , typTest "Array(T)[]" (TEArr (app "Array" [t]))

  , typTest "[]" (tup [])
  , typTest "[A]" (tup [a])
  , typTest "[A, B]" (tup [a, b])
  , typTest "[A, B][]" (TEArr (tup [a, b]))
  , typTest "[A | B, C]" (tup [un a b, c])

  , typTest "{}" (TEObj [])
  , typTest "{ a: A }" (TEObj [fld "a" a])
  , typTest "{ a: A, b?: B }" (TEObj [fld "a" a, opt "b" b])
  , typTest "{ a: { b: B } }" (TEObj [fld "a" (TEObj [fld "b" b])])
  , typTest "{ a: A[] }" (TEObj [fld "a" (TEArr a)])

  , typTest "fn() => A" (fun [] a)
  , typTest "fn(x: A) => B" (fun [prm "x" a] b)
  , typTest "fn(x: A, y?: B) => C" (fun [prm "x" a, oprm "y" b] c)
  , typTest "fn(x: A) => B | C" (fun [prm "x" a] (un b c))
  , typTest "fn(x: A) => fn(y: B) => C" (fun [prm "x" a] (fun [prm "y" b] c))
  , typTest "fn(f: fn(x: A) => B) => C" (fun [prm "f" (fun [prm "x" a] b)] c)
  , typTest "(fn(x: A) => B) | C" (un (fun [prm "x" a] b) c)
  , typTest "(fn() => A)[]" (TEArr (fun [] a))

  , typTest "forall T. fn(x: T) => T" (TEFunc [Ident "T"] [prm "x" t] t)
  , typTest "forall K V. fn(m: Map(K, V), k: K) => V | undefined"
      (TEFunc [Ident "K", Ident "V"]
        [prm "m" (app "Map" [ref "K", ref "V"]), prm "k" (ref "K")]
        (un (ref "V") (ref "undefined")))
  , typTest "fn(f: forall T. fn(x: T) => T) => number" (fun [prm "f" (TEFunc [Ident "T"] [prm "x" t] t)] (ref "number"))
  , typTest "forall T. fn(x: T) => T | null" (TEFunc [Ident "T"] [prm "x" t] (un t (TELit (LNull "null"))))
  , typTest "(forall T. fn(x: T) => T)[]" (TEArr (TEFunc [Ident "T"] [prm "x" t] t))
  , typTest "(forall T. fn() => T) | A" (un (TEFunc [Ident "T"] [] t) a)

  , typTest "forall K V.\n  fn(m: Map(K, V)) => K[]" (TEFunc [Ident "K", Ident "V"] [prm "m" (app "Map" [ref "K", ref "V"])] (TEArr (ref "K")))

  , typFail "forall. fn() => A"
  , typFail "forall T fn(x: T) => T"
  , typFail "forall T. A"
  , typFail "A | forall T. fn() => T"

  , typTest "A\n  | B\n  | C" (un (un a b) c)
  , typTest "{\n  a: A,\n  b: B\n}" (TEObj [fld "a" a, fld "b" b])
  , typTest "fn(x: A)\n  => B" (fun [prm "x" a] b)

  , typTest "Promise(Array(User))[] | null" (un (TEArr (app "Promise" [app "Array" [ref "User"]])) (TELit (LNull "null")))

  , typTest "{ id: number, tags?: string[], owner: { name: string } }"
      (TEObj [ fld "id" (ref "number")
             , opt "tags" (TEArr (ref "string"))
             , fld "owner" (TEObj [fld "name" (ref "string")]) ])

  , typTest "fn(req: Request, next?: fn() => undefined) => Promise(Response)"
      (fun [prm "req" (ref "Request"), oprm "next" (fun [] (ref "undefined"))]
           (app "Promise" [ref "Response"]))

  , typTest "[string, number] & { length: 2 }"
      (bi (tup [ref "string", ref "number"]) (TEObj [fld "length" (TELit (LNum "2"))]))

  , typTest "A & B[] | C"     (un (bi a (TEArr b)) c)
  , typTest "(A | B)[][]"     (TEArr (TEArr (un a b)))

  , typTest "{\n  onClick: fn(e: MouseEvent) => boolean,\n  items: (Item | Group)[]\n}"
      (TEObj [ fld "onClick" (fun [prm "e" (ref "MouseEvent")] (ref "boolean"))
             , fld "items" (TEArr (un (ref "Item") (ref "Group"))) ])

  , typFail "A | fn() => B"

  , typTest "fn(x: A) => void" (fun [prm "x" a] (ref "void"))

  , typFail "A |"
  , typFail "A\n| B"
  , typFail "Foo<T>"
  , typFail "fn(x) => A"
  , typFail "forall T. A"
  , typFail "{ a }"
  , typFail "[A, B"
  ]