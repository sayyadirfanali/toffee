{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Lexer where

import Control.Monad
import Data.Char
import qualified Data.List as DL
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug
import Utils
import Data.Maybe (fromMaybe, maybeToList)
import Data.Ord (Down(..))

type Lexer = Parsec Void String

instance ShowErrorComponent String where
  showErrorComponent :: String -> String
  showErrorComponent = show

skipSpace :: Lexer ()
skipSpace = L.space (void $ oneOf [' ', '\t', '\n']) (L.skipLineComment "//") empty

lexeme :: Lexer a -> Lexer a
lexeme p = p <* skipSpace

iden :: Lexer Char
iden = letterChar <|> digitChar <|> char '_'

symbol :: String -> Lexer String
symbol s = lexeme (string s <* notFollowedBy iden)

data TokInfo = TokInfo
  { tokStart :: SourcePos,
    tokStr   :: String,
    tokVal   :: Tok
  }
  deriving (Eq, Ord, Show)

instance VisualStream [TokInfo] where
  showTokens :: Proxy [TokInfo] -> NE.NonEmpty (Token [TokInfo]) -> String
  showTokens Proxy = DL.intercalate ", " . NE.toList . fmap (show . tokVal)

  tokensLength :: Proxy [TokInfo] -> NE.NonEmpty (Token [TokInfo]) -> Int
  tokensLength Proxy xs = sum (fmap (length . tokStr) xs)

data Tok
  = TLit Lit
  | TIden String
  | TSym String
  | TBracket Char
  | TKey String
  | TSemiColon
  | TEOF
  | TLayBrace Int
  | TLayAngle Int
  deriving (Eq, Ord, Show)

data Lit
  = LNull String
  | LBool String
  | LNum String
  | LStr String
  | LReg String
  deriving (Eq, Ord, Show)

tokDat :: Tok -> String
tokDat (TLit (LNull s)) = s
tokDat (TLit (LBool s)) = s
tokDat (TLit (LNum s))  = s
tokDat (TLit (LStr s))  = s
tokDat (TLit (LReg s))  = s
tokDat (TIden s)        = s
tokDat (TSym s)         = s
tokDat (TBracket c)     = [c]
tokDat (TKey s)         = s
tokDat TSemiColon       = ";"
tokDat _                = []

nullL :: Lexer Tok
nullL = label "null" $ do
  try $ symbol "null"
  return $ TLit (LNull "null")

boolL :: Lexer Tok
boolL = label "bool" $ do
  b <- try $ choice ([symbol "true", symbol "false"])
  return $ TLit (LBool b)

numL :: Lexer Tok
numL = label "number" $ do
  d <- some digitChar
  f <- optional frac
  e <- optional exp
  return $ TLit (LNum $ d ++ fromMaybe [] f ++ fromMaybe [] e)
  where
    frac = (:) <$> char '.' <*> some digitChar
    exp = do
      x <- oneOf "eE"
      y <- optional $ oneOf "+-" 
      z <- some digitChar
      return $ x : maybeToList y ++ z

strL :: Lexer Tok
strL = label "string" $ do
  let quote = char '"' <|> char '\'' <|> char '`'
  c <- quote
  s <- manyTill L.charLiteral (char c)
  return $ TLit (LStr s)

litL :: Lexer Tok
litL = choice [nullL, boolL, numL, strL]

idenL :: Lexer Tok
idenL = label "identifier" $ do
  x <- satisfy (\c -> isAlpha c || c == '_')
  xs <- many iden
  return $ TIden (x : xs)

ops = DL.sortOn (Down . length) . words $
  "... ?. ?? ** == /= <= => >= && || -> :: += -= *= %= **= ??= ||= &&= << >> >>> = - + * / % < > ! : , . ? & | ^"

symL :: Lexer Tok
symL = label "symbol" $ do
  s <- choice (fmap string ops)
  return $ TSym s

keys = words "instanceof satisfies await const typeof where else then void new let as do fn if"

keyL :: Lexer Tok
keyL = label "keyword" $ do
  k <- try $ choice (fmap symbol keys)
  return $ TKey k

braL :: Lexer Tok
braL = label "bracket" $ do
  bra <- satisfy (flip elem "({[]})")
  return $ TBracket bra

semiL :: Lexer Tok
semiL = label "semicolon" $ do
  string ";"
  return TSemiColon

mkTokInfo :: Lexer Tok -> Lexer TokInfo
mkTokInfo p = do
  s <- getSourcePos
  t <- p
  return TokInfo { tokStart = s, tokStr = tokDat t, tokVal = t }

lexer :: Lexer [TokInfo]
lexer = do
  skipSpace
  tokInfo <- many . (mkTokInfo . lexeme) . choice $ [ keyL, symL, braL, semiL, litL, idenL ]
  pos     <- getSourcePos
  eof
  return $ tokInfo ++ [TokInfo { tokStart = pos, tokStr = [], tokVal = TEOF }]

runLex = runParser (lexer) ""