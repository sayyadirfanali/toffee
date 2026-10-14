module Parser where

import Data.Void
import Text.Megaparsec
import qualified Data.Set as Set

import Lexer

type Parser = Parsec Void [TokInfo]

satisfyP :: (Tok -> Bool) -> Parser TokInfo
satisfyP p = satisfy (p . tokVal)

matchP :: (Tok -> Maybe a) -> Parser a
matchP f = token (f . tokVal) Set.empty

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

parenP :: Parser a -> Parser a
parenP = between (bracketP '(') (bracketP ')')

curlyP :: Parser a -> Parser a
curlyP = between (bracketP '{') (bracketP '}')

squareP :: Parser a -> Parser a
squareP = between (bracketP '[') (bracketP ']')

eofP :: Parser Tok
eofP = singleP TEOF

runParse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
runParse p = runParser p ""
