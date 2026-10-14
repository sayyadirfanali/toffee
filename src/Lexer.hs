{- HLINT ignore "Functor law" -}
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
import Data.Maybe (fromMaybe, isJust, maybeToList)
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
symbol s = string s <* notFollowedBy (satisfy identBody)

data TokInfo = TokInfo
  { tokStart    :: SourcePos
  , tokEnd      :: SourcePos
  , tokStartOff :: Int
  , tokEndOff   :: Int
  , tokStr      :: String
  , tokVal      :: Tok
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
  b <- try $ choice [symbol "true", symbol "false"]
  return $ TLit (LBool b)

validSeps :: String -> Bool
validSeps ds = take 1 ds /= "_" && take 1 (reverse ds) /= "_" && not ("__" `DL.isInfixOf` ds)

digitsL :: (Char -> Bool) -> Lexer String
digitsL isDig = do
  ds <- some (satisfy (\c -> isDig c || c == '_'))
  if validSeps ds
    then return ds
    else fail "a numeric separator `_` must sit between two digits"

baseDigit :: Char -> Char -> Bool
baseDigit b c
  | b `elem` "xX" = isHexDigit c
  | b `elem` "oO" = isOctDigit c
  | otherwise     = c == '0' || c == '1'

prefixedL :: Lexer String
prefixedL = do
  b <- try (char '0' *> oneOf "xXoObB")
  ds <- digitsL (baseDigit b) <?> "digit"
  n <- optional (char 'n')
  return ('0' : b : ds ++ maybeToList n)

fractionL :: Lexer String
fractionL = do
  char '.'
  ds <- digitsL isDigit <?> "digit after the decimal point"
  return ('.' : ds)

exponentL :: Lexer String
exponentL = do
  e <- oneOf "eE"
  sign <- optional (oneOf "+-")
  ds <- digitsL isDigit <?> "exponent digit"
  return (e : maybeToList sign ++ ds)

decimalL :: Lexer String
decimalL = do
  lookAhead digitChar
  i <- digitsL isDigit
  when (length i > 1 && take 1 i == "0") (fail "a number cannot start with a leading zero")
  f <- optional fractionL
  e <- optional exponentL
  n <- if isJust f || isJust e then return Nothing else optional (char 'n')
  return (i ++ fromMaybe "" f ++ fromMaybe "" e ++ maybeToList n)

dotL :: Lexer String
dotL = do
  try (char '.' <* lookAhead digitChar)
  ds <- digitsL isDigit
  e <- optional exponentL
  return ('.' : ds ++ fromMaybe "" e)

numEndL :: Lexer ()
numEndL = do
  next <- optional (lookAhead (satisfy identBody))
  case next of
    Just _  -> fail "a number cannot be followed directly by a letter or digit"
    Nothing -> return ()

numBodyL :: Lexer String
numBodyL = do
  s <- choice [prefixedL, decimalL, dotL]
  numEndL
  return s

numL :: Lexer Tok
numL = label "number" $ do
  o <- getOffset
  s <- region (setErrorOffset o) numBodyL
  return $ TLit (LNum s)

hexN :: Int -> Lexer Char
hexN n = toEnum <$> (read . ("0x" ++) <$> count n hexDigitChar)

hexVar :: Lexer Char
hexVar = do
  ds <- some hexDigitChar
  let v = read ("0x" ++ ds) :: Integer
  if v > 0x10FFFF
    then fail "a unicode escape cannot exceed \\u{10FFFF}"
    else return (toEnum (fromInteger v))

uEsc :: Lexer Char
uEsc = (char '{' *> hexVar <* char '}') <|> hexN 4

zeroEsc :: Lexer Char
zeroEsc = do
  char '0'
  (digitChar *> fail "octal escapes are not allowed") <|> return '\0'

octalEsc :: Lexer Char
octalEsc = oneOf "123456789" *> fail "octal escapes are not allowed"

escBody :: Lexer Char
escBody = choice
  [ '\n' <$ char 'n'
  , '\t' <$ char 't'
  , '\r' <$ char 'r'
  , '\b' <$ char 'b'
  , '\f' <$ char 'f'
  , '\v' <$ char 'v'
  , zeroEsc
  , octalEsc
  , char 'x' *> hexN 2
  , char 'u' *> uEsc
  , anySingle
  ]

lineTermL :: Lexer ()
lineTermL = choice
  [ void (string "\r\n")
  , void (char '\n')
  , void (char '\r')
  , void (char '\x2028')
  , void (char '\x2029')
  ]

escapeL :: Lexer String
escapeL = do
  char '\\'
  ("" <$ lineTermL) <|> ((: []) <$> escBody)

strPiece :: Char -> Lexer String
strPiece q = choice
  [ escapeL
  , "\n" <$ string "\r\n"
  , "\n" <$ char '\r'
  , (: []) <$> satisfy (\c -> c /= q && c /= '\\')
  ]

strL :: Lexer Tok
strL = label "string" $ do
  q <- char '"' <|> char '\'' <|> char '`'
  pieces <- manyTill (strPiece q) (char q)
  return $ TLit (LStr (concat pieces))

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
  , "! ~ **"
  , "++ --"
  , "* / %"
  , "+ -"
  , "<< >> >>>"
  , "< <= > >= == !="
  , "& ^ |"
  , "&& || ??"
  , "=>"
  , "= += -= *= /= %= **= <<= >>= >>>= &= |= ^= &&= ||= ??="
  , ", :"
  , "? -> ::"
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
  bra <- satisfy (`elem` "({[]})")
  return $ TBracket bra

semiL :: Lexer Tok
semiL = label "semicolon" $ do
  string ";"
  return TSemiColon

mkTokInfo :: Lexer Tok -> Lexer TokInfo
mkTokInfo p = do
  s  <- getSourcePos
  so <- getOffset
  t  <- p
  e  <- getSourcePos
  eo <- getOffset
  return TokInfo { tokStart = s, tokEnd = e, tokStartOff = so, tokEndOff = eo, tokStr = tokDat t, tokVal = t }

lexer :: Lexer [TokInfo]
lexer = do
  skipSpace
  tokInfo <- many . lexeme . mkTokInfo . choice $ [ keyL, litL, symL, braL, semiL, privL, idenL ]
  pos <- getSourcePos
  off <- getOffset
  eof
  return $ tokInfo ++ [TokInfo { tokStart = pos, tokEnd = pos, tokStartOff = off, tokEndOff = off, tokStr = [], tokVal = TEOF }]

runLex :: String -> Either (ParseErrorBundle String Void) [TokInfo]
runLex = runParser lexer ""

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
