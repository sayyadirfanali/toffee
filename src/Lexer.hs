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
import Data.Maybe (maybeToList)
import Data.Ord (Down(..))

identHead :: Char -> Bool
identHead c = isAlpha c || elem c "_$"

identBody :: Char -> Bool
identBody c = identHead c || isDigit c

type Lexer = Parsec Void String

skipSpace :: Lexer ()
skipSpace = L.space (void $ oneOf [' ', '\t', '\r', '\n']) (L.skipLineComment "//") empty

lexeme :: Lexer a -> Lexer a
lexeme p = p <* skipSpace

symbol :: String -> Lexer String
symbol s = lexeme (string s <* notFollowedBy (satisfy identBody))

data TokInfo = TokInfo { tokStart :: SourcePos, tokStr :: String, tokVal :: Tok }
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
  | TPriv String
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
  | LReg String String
  deriving (Eq, Ord, Show)

litDat :: Lit -> String
litDat (LNum s) = s
litDat (LStr s) = show s
litDat (LBool s) = s
litDat (LNull s) = s
litDat (LReg s t) = "r\"" <> s <> "\"" <> t

tokDat :: Tok -> String
tokDat (TLit (LNull s)) = s
tokDat (TLit (LBool s)) = s
tokDat (TLit (LNum s)) = s
tokDat (TLit (LStr s)) = s
tokDat (TLit (LReg s t)) = "r\"" <> s <> "\"" <> t
tokDat (TIden s) = s
tokDat (TPriv s) = s
tokDat (TSym s) = s
tokDat (TBracket c) = [c]
tokDat (TKey s) = s
tokDat TSemiColon = ";"
tokDat _ = []

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
  n <- try prefixL <|> decimalL
  return $ TLit (LNum n)
  where
    prefixL = do
      z <- char '0'
      b <- oneOf "xXoObB"
      ds <- some (hexDigitChar <|> char '_')
      n <- optional (char 'n')
      return $ z : b : ds ++ maybe [] pure n
    decimalL = do
      d <- digitL
      f <- optional ((:) <$> char '.' <*> digitL)
      e <- optional expL
      n <- optional (char 'n')
      return $ d <> maybe [] id f <> maybe [] id e <> maybe [] pure n
    digitL  = (:) <$> digitChar <*> many (digitChar <|> char '_')
    expL = do
      x <- oneOf "eE"
      y <- optional (oneOf "+-")
      z <- digitL
      return $ x : maybeToList y ++ z

escChar :: Lexer Char
escChar = char '\\' *> (esc <|> anySingle)
  where
    esc = choice
      [ '\n' <$ char 'n'
      , '\t' <$ char 't'
      , '\r' <$ char 'r'
      , '\b' <$ char 'b'
      , '\f' <$ char 'f'
      , '\v' <$ char 'v'
      , '\0' <$ try (char '0' <* notFollowedBy digitChar)
      , char 'x' *> hexN 2
      , char 'u' *> uEsc
      ]
    uEsc = (char '{' *> hexVar <* char '}') <|> hexN 4
    hexN n = toEnum <$> (read . ("0x" ++) <$> count n hexDigitChar)
    hexVar = toEnum <$> (read . ("0x" ++) <$> some hexDigitChar)

strChar :: Char -> Lexer Char
strChar q = escChar <|> satisfy (\c -> c /= q && c /= '\\')

strL :: Lexer Tok
strL = label "string" $ do
  q <- char '"' <|> char '\'' <|> char '`'
  s <- manyTill (strChar q) (char q)
  return $ TLit (LStr s)

litL :: Lexer Tok
litL = label "literal" $ choice [nullL, boolL, numL, regL, strL]

identAuxL :: Lexer String
identAuxL = do
    x <- satisfy identHead
    xs <- many (satisfy identBody)
    return (x : xs)

idenL :: Lexer Tok
idenL = label "identifier" $
  TIden <$> identAuxL

privL :: Lexer Tok
privL = label "private identifier" $
  TPriv <$> (char '#' *> identAuxL)

regL :: Lexer Tok
regL = label "regex" $ do
  _ <- try (char 'r' <* lookAhead (char '"'))
  _ <- char '"'
  body  <- concat <$> manyTill regChar (char '"')
  flags <- many (oneOf "gimsuydv")
  return $ TLit (LReg body flags)
  where
    regChar = try (['"'] <$ string "\\\"") <|> ((:[]) <$> satisfy (/= '"'))

ops :: [String]
ops = DL.sortOn (Down . length) . concatMap words $
  [ "... ?. ."
  , "! **"
  , "* / %"
  , "+ -"
  , "<< >> >>>"
  , "< <= > >= == !="
  , "& ^ |"
  , "&& || ??"
  , "=>"
  , "= += -= *= /= %= **= <<= >>= >>>= &= |= ^= &&= ||= ??="
  , ", :"
  , "? -> ::" -- reserved for future use
  ]

symL :: Lexer Tok
symL = label "symbol" $ do
  s <- choice (fmap string ops)
  return $ TSym s

keys :: [String]
keys = DL.sortOn (Down . length) . words $
  "instanceof \
  \ continue \
  \ extends finally \
  \ delete import export return typeof \
  \ await break catch class const throw where while \
  \ else then void \
  \ for let new try \
  \ do fn if in"

keyL :: Lexer Tok
keyL = label "keyword" $ do
  k <- choice (fmap (try . symbol) keys)
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
  tokInfo <- many . (mkTokInfo . lexeme) . choice $ [ keyL, symL, braL, semiL, litL, privL, idenL ]
  pos <- getSourcePos
  eof
  return $ tokInfo ++ [TokInfo { tokStart = pos, tokStr = [], tokVal = TEOF }]

runLex :: String -> Either (ParseErrorBundle String Void) [TokInfo]
runLex = runParser (lexer) ""

-- auxiliaries needed in both lexer and layout tests
sm :: Tok
sm = TSemiColon

oa :: Tok
oa = TBracket '<'

ca :: Tok
ca = TBracket '>'

ob :: Tok
ob = TBracket '{'

cb :: Tok
cb = TBracket '}'

op :: Tok
op = TBracket '('

cp :: Tok
cp = TBracket ')'

oq :: Tok
oq = TBracket '['

cq :: Tok
cq = TBracket ']'

dk :: Tok
dk = TKey "do"

wk :: Tok
wk = TKey "where"