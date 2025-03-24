{-# Language FlexibleInstances #-}
{-# Language RecordWildCards #-}
{-# Language TypeFamilies #-}

module Lexer where

import Control.Monad.State
import Data.Char
import Data.Void
import Data.Proxy
import qualified Data.List.NonEmpty as NE
import qualified Data.List          as DL
 
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer as L

import Utils

type Lexer = ParsecT String String (StateT Stack IO)

instance ShowErrorComponent String where
  showErrorComponent = show

type Level = Int
data Stack = Stack { unStack :: [Level] } deriving Show

getStack :: Lexer [Level]
getStack = gets unStack

setStack :: [Level] -> Lexer ()
setStack stack = modify (\st -> st { unStack = stack })

pushLevel :: Int -> Lexer ()
pushLevel x = do
  stack <- getStack
  setStack (x : stack)

popLevel :: Lexer ()
popLevel = do
  stack <- getStack
  case stack of
    []       -> setStack []
    (_ : xs) -> setStack xs

getRef :: Lexer Int
getRef = do
  stack  <- getStack
  return $ if null stack then 1 else head stack

getCur :: Lexer Int
getCur = unPos <$> L.indentLevel 

skipSpace :: Lexer ()
skipSpace = L.space (void . choice . fmap char $ [' ', '\t']) (L.skipLineComment "//") empty
 
lexeme :: Lexer a -> Lexer a
lexeme p = p <* skipSpace where

skipSpaceNL :: Lexer ()
skipSpaceNL = L.space space1 (L.skipLineComment "//") empty

lexemeNL :: Lexer a -> Lexer a
lexemeNL p = p <* skipSpaceNL where

symbol :: String -> Lexer String
symbol s = lexeme (string s <* notFollowedBy alphaNumChar)

symbolNL :: String -> Lexer String
symbolNL s = lexemeNL (string s <* notFollowedBy alphaNumChar)

data TokInfo = TokInfo {
    tokStart   :: SourcePos
  , tokLen     :: Int
  , tokVal     :: Tok
  } deriving (Eq, Ord, Show)

instance VisualStream [TokInfo] where
  showTokens   Proxy    = DL.intercalate ", " . NE.toList . fmap (show . tokVal)
  tokensLength Proxy xs = sum (fmap tokLen xs)

data Tok
  = TLit      Lit
  | TIden     String
  | TSym      String
  | TBracket  Char
  | TKey      String
  | TSemiColon
  | TEOF
  deriving (Eq, Ord, Show)

data Lit
   = LNull String
   | LBool String
   | LNum  String
   | LStr  String
   | LReg  String
   deriving (Eq, Ord, Show)

nullL :: Lexer [Tok]
nullL = label "null" $ do
  symbol "null"
  return [TLit (LNull "null")]

boolL :: Lexer [Tok]
boolL = label "bool" $ do
  b <- choice (fmap symbol [ "true", "false" ])
  return [TLit (LBool b)]
 
numL :: Lexer [Tok]
numL = label "number" $ do
  d <- some dig
  f <- many frac
  e <- many exp
  return [TLit (LNum (concat (concat [d] <> f <> e)))]
  where
    dig  = some digitChar
    frac = (:) <$> char '.' <*> some digitChar
    exp  = do
      x <- choice (fmap string ["e, E"])
      y <- choice (fmap string ["+", "-", ""])
      z <- some digitChar
      return (x <> y <> z)

strL :: Lexer [Tok]
strL = label "string" $ do
  let quote = char '"' <|> char '\'' <|> char '`'
  c <- quote
  s <- manyTill L.charLiteral (char c)
  return [TLit (LStr s)]

litL :: Lexer [Tok]
litL = choice [ nullL, boolL, numL, strL ]

identL :: Lexer [Tok]
identL = label "identifier" $ do
  x  <- satisfy (\c -> isAlpha c || c == '_')
  xs <- many (letterChar <|> digitChar)
  return [TIden (x : xs)]

symL :: Lexer [Tok]
symL = label "symbol" $ do
  s <- choice (fmap string ops)
  return [TSym s]

braL :: Lexer [Tok]
braL = label "bracket" $ do
  bra <- satisfy (flip elem "({[]})")
  cur <- getCur
  case bra of
    '{' -> do
      pushLevel 0
      return [TBracket '{']
    '}' -> do
      ref <- getRef
      popLevel
      if ref > 1 then fail "Wrong Bracket"
      else return [TSemiColon, TBracket '}']
    _   -> return [TBracket bra]

keyL :: Lexer [Tok]
keyL = label "keyword" $ do
  k <- choice (fmap symbol keys)
  return [TKey k]

semiL :: Lexer [Tok]
semiL = label "semicolon" $ do
  string ";"
  return [TSemiColon]

blockL :: Lexer [Tok]
blockL = do
  k     <- choice (fmap (try . symbolNL) [ "do", "where" ])
  isEOF <- atEnd
  case isEOF of
    True  -> fail "Hanging Block"
    False -> do
      cur   <- getCur
      isBra <- isFollowedBy (char '{')
      case isBra of
        True -> do
          char '{'
          pushLevel 0
          return [TKey k, TBracket '{']
        False -> do
          pushLevel cur
          return [TKey k, TBracket '<']

mkEnds :: [Level] -> [Tok]
mkEnds st = concatMap (const [TSemiColon, TBracket '>']) st

lineL :: Lexer [Tok]
lineL = label "eol" $ do
  try (lexemeNL eol)
  isEnd <- atEnd
  stack <- getStack
  ref   <- getRef
  cur   <- getCur
  case isEnd of
    True  -> if ref == 0 then fail "Indentation Error" else return (mkEnds stack)
    False -> do
      case compare cur ref of
        GT -> return []
        EQ -> return [TSemiColon]
        LT -> do
          if   not (elem cur stack)
          then fail "Indentation Error"
          else do
            let (remove, keep) = span (/= cur) stack -- TODO: check if some explicit blocks are open
            setStack keep
            return (mkEnds remove)

mkTokInfo :: Lexer [Tok] -> Lexer [TokInfo]
mkTokInfo p = do
  s  <- getSourcePos
  ts <- p
  e  <- getSourcePos
  return $ fmap (\t -> TokInfo { tokStart = s, tokLen = 1, tokVal = t }) ts

ls = fmap (lexeme . uncurry dbg)
  [
    ("key",    keyL)
  , ("block",  blockL)
  , ("sym",    symL)
  , ("bra",    braL)
  , ("semi",   semiL)
  , ("lit",    litL)
  , ("ident",  identL)
  ]

lexer :: Lexer [TokInfo]
lexer = mkTokInfo . fmap concat . many $ p
  where p = choice ls <|> dbg "line" lineL

runLex :: String -> IO (Either (ParseErrorBundle String String) [TokInfo])
runLex s = evalStateT (runParserT (lexer <* eof) "" s) (Stack [])

keys =
  [
      "const"
    , "else"
    , "then"
    , "let"
    , "fn"
    , "if"
  ]

ops =
  [
      "==="
    , "->"
    , "::"
    , "++"
    , "--"
    , "=="
    , "=<"
    , ">="
    , "="
    , "+"
    , "-"
    , "*"
    , "/"
    , ":"
    , ","
    , "."
    , "<"
    , ">"
  ]
