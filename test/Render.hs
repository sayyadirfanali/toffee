module Render (render) where

import Expr
import Lexer (litDat)

render :: Expr -> String
render = \case
  ELit l            -> litDat l
  EIden (Ident s)   -> s
  EPriv (Ident s)   -> '#' : s
  EArr es           -> wrap "[" "]" (fmap element es)
  EObj fs           -> wrap "{" "}" (fmap field fs)
  ELam sig body     -> form (if fsIsAsync sig then "async-fn" else "fn") [params sig, render body]
  EMem e m          -> form "." [render e, mem m]
  EOptMem e m       -> form "?." [render e, mem m]
  EApp f as         -> form (render f) (fmap element as)
  EOptApp f as      -> form "?.()" (render f : fmap element as)
  EIndex e i        -> form "[]" [render e, render i]
  EOptIndex e i     -> form "?.[]" [render e, render i]
  ENew e as         -> form "new" (render e : fmap element as)
  ENewTarget        -> "new.target"
  EUnOp o e         -> form (unop2str o) [render e]
  EBinOp o a b      -> form (binop2str o) [render a, render b]
  EAssign o a b     -> form (assignop2str o) [render a, render b]
  EThrow e          -> form "throw" [render e]
  EImport c m       -> form "import" (impClause c <> modRef m)
  EImportCall as    -> form "import()" (fmap element as)
  EImportMeta       -> "import.meta"
  EExportDecl e     -> form "export" [render e]
  EExportDefault e  -> form "export-default" [render e]
  EExport c mm      -> form "export" (expClause c : maybe [] modRef mm)
  EClass cd         -> form "class" (classParts cd)
  EDecl t p e       -> form (if t == LetDecl then "let" else "const") [render p, render e]
  EBlock _ (BlockKey k) xs -> form k (fmap render xs)
  EIfThen c t e     -> form "if" (fmap render [c, t, e])
  ETry b mc mf      -> form "try" (render b : catchPart mc <> finallyPart mf)
  EFor aw p it b    -> form (if aw then "for-await" else "for") (fmap render [p, it, b])
  EWhile c b        -> form "while" [render c, render b]
  EBreak            -> form "break" []
  EContinue         -> form "continue" []
  EReturn me        -> form "return" (maybe [] (pure . render) me)

form :: String -> [String] -> String
form h xs = "(" <> unwords (h : xs) <> ")"

wrap :: String -> String -> [String] -> String
wrap o c xs = o <> unwords xs <> c

element :: Elem -> String
element (ElemPlain e)  = render e
element (ElemSpread e) = "..." <> render e

field :: ObjField -> String
field (OFSpread e)              = "..." <> render e
field (OFKeyVal k v)            = key k <> ":" <> render v
field (OFShort (Ident s))       = s
field (OFShortInit (Ident s) e) = s <> "=" <> render e
field (OFMethod k sig b)        = methodForm k sig b

key :: PropKey -> String
key (PKMem m)  = mem m
key (PKLit l)  = litDat l
key (PKComp e) = "[" <> render e <> "]"

mem :: MemName -> String
mem (MNPub s)  = s
mem (MNPriv s) = '#' : s

params :: FuncSig -> String
params sig = "(" <> unwords (fmap param (fsParams sig)) <> ")"
  where
    param (LPPlain p Nothing)  = render p
    param (LPPlain p (Just d)) = render p <> "=" <> render d
    param (LPRest p)           = "..." <> render p

methodForm :: PropKey -> FuncSig -> Expr -> String
methodForm k sig b = form (if fsIsAsync sig then "async-method" else "method") [key k, params sig, render b]

classParts :: ClassDecl -> [String]
classParts (ClassDecl name ext mems) =
  maybe [] (\(Ident s) -> [s]) name
  <> maybe [] (\e -> [form "extends" [render e]]) ext
  <> [wrap "{" "}" (fmap member mems)]
  where
    member m = (if cmStatic m then \s -> form "static" [s] else id) $ case m of
      CMField _ k mv    -> form "field" (key k : maybe [] (pure . render) mv)
      CMethod _ k sig b -> methodForm k sig b

impClause :: ImportClause -> [String]
impClause (ImportClause md mb) = maybe [] (\(Ident d) -> [d]) md <> maybe [] binds mb
  where
    binds (ImpNamed ss)            = [wrap "{" "}" (fmap spec ss)]
    binds (ImpNamespace (Ident n)) = ['*' : n]
    spec (ImportSpec n Nothing)          = n
    spec (ImportSpec n (Just (Ident a))) = form "as" [n, a]

expClause :: ExportClause -> String
expClause (ExpNamed ss) = wrap "{" "}" (fmap spec ss)
  where
    spec (ExportSpec n Nothing)  = n
    spec (ExportSpec n (Just a)) = form "as" [n, a]
expClause ExpAll        = "*"
expClause (ExpAllAs n)  = '*' : n

-- empty attribute lists render as absent, since `with {}` means nothing
modRef :: ModuleRef -> [String]
modRef (ModuleRef src attrs) = show src : [ form "with" [wrap "{" "}" (fmap attr attrs)] | not (null attrs) ]
  where
    attr (k, v) = k <> ":" <> show v

catchPart :: Maybe TryCatch -> [String]
catchPart Nothing                  = []
catchPart (Just (TryCatch mb h))   = [form "catch" (maybe [] (pure . render) mb <> [render h])]

finallyPart :: Maybe Expr -> [String]
finallyPart = maybe [] (\f -> [form "finally" [render f]])