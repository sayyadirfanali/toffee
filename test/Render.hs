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
  ENew e            -> form "new" [render e]
  EUnOp o e         -> form (unop2str o) [render e]
  EBinOp o a b      -> form (binop2str o) [render a, render b]
  EAssign o a b     -> form (assignop2str o) [render a, render b]
  EThrow e          -> form "throw" [render e]
  EImport c src     -> form "import" (impClause c <> [show src])
  EImportCall as    -> form "import()" (fmap element as)
  EImportMeta       -> "import.meta"
  EExportDecl e     -> form "export" [render e]
  EExport c mfrom   -> form "export" (expClause c : maybe [] (pure . show) mfrom)
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

key :: PropKey -> String
key (PKMem m)  = mem m
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

classParts :: ClassDecl -> [String]
classParts (ClassDecl name ext mems) =
  maybe [] (\(Ident s) -> [s]) name
  <> maybe [] (\e -> [form "extends" [render e]]) ext
  <> [wrap "{" "}" (fmap member mems)]
  where
    member m = (if cmStatic m then \s -> form "static" [s] else id) $ case m of
      CMField _ k mv    -> form "field" (key k : maybe [] (pure . render) mv)
      CMethod _ k sig b -> form (if fsIsAsync sig then "async-method" else "method") [key k, params sig, render b]

impClause :: ImportClause -> [String]
impClause ImpBare                  = []
impClause (ImpNamed ss)            = [wrap "{" "}" (fmap spec ss)]
  where
    spec (ImportSpec n Nothing)          = n
    spec (ImportSpec n (Just (Ident a))) = form "as" [n, a]
impClause (ImpNamespace (Ident n)) = ['*' : n]

expClause :: ExportClause -> String
expClause (ExpNamed ss) = wrap "{" "}" (fmap spec ss)
  where
    spec (ExportSpec n Nothing)  = n
    spec (ExportSpec n (Just a)) = form "as" [n, a]
expClause ExpAll        = "*"
expClause (ExpAllAs n)  = '*' : n

catchPart :: Maybe TryCatch -> [String]
catchPart Nothing                  = []
catchPart (Just (TryCatch mb h))   = [form "catch" (maybe [] (pure . render) mb <> [render h])]

finallyPart :: Maybe Expr -> [String]
finallyPart = maybe [] (\f -> [form "finally" [render f]])