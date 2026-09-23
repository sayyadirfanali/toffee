module Parser where

import Data.Void
import Text.Megaparsec
import qualified Data.Set as Set

import Utils
import Lexer

type Parser = ParsecT Void [TokInfo] IO

satisfyP :: (Tok -> Bool) -> Parser TokInfo
satisfyP p = satisfy (p . tokVal)

matchP :: (Tok -> Maybe a) -> Parser a
matchP f = token (f . tokVal) Set.empty

posP :: Parser SourcePos
posP = tokStart <$> lookAhead anySingle

singleP :: Tok -> Parser Tok
singleP t = (tokVal <$> satisfyP (== t)) <?> tokDat t

symP :: String -> Parser String
symP s = s <$ singleP (TSym s)

keyP :: String -> Parser String
keyP k = k <$ singleP (TKey k)

stringP :: Parser String
stringP = matchP getStr
  where 
    getStr (TLit (LStr s)) = Just s
    getStr _               = Nothing

bracketP :: Char -> Parser Char
bracketP c = c <$ singleP (TBracket c)

commaP :: Parser String
commaP = symP ","

semicolonP :: Parser Tok
semicolonP = singleP TSemiColon

eofP :: Parser Tok
eofP = label "eof" $ singleP TEOF

parenP :: Parser a -> Parser a
parenP = between (bracketP '(') (bracketP ')')

curlyP :: Parser a -> Parser a
curlyP = between (bracketP '{') (bracketP '}')

squareP :: Parser a -> Parser a
squareP = between (bracketP '[') (bracketP ']')

runParse p = runParserT p ""