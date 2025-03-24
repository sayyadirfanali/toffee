module Parser where

import Data.Void
import Text.Megaparsec

import Utils
import Lexer

type Parser = ParsecT Void [TokInfo] IO

satisfyP :: (Tok -> Bool) -> Parser Tok
satisfyP p = satisfy (p . tokVal) >>= return . tokVal

singleP :: Tok -> Parser Tok
singleP t = satisfyP (== t)

consP :: String -> Tok -> Bool
consP  "Lit"         (TLit _)      = True
consP  "Sym"         (TSym _)      = True
consP  "Key"         (TKey _)      = True
consP  "Iden"        (TIden _)     = True
consP  "Bracket"     (TBracket _)  = True
consP  "SemiColon"   TSemiColon    = True
consP  "EOF"         TEOF          = True
consP  _ _                         = False

symP :: String -> Parser Tok
symP s = satisfyP (== TSym s)

keyP :: String -> Parser Tok
keyP s = satisfyP (== TKey s)

bracketP ::  Char -> Parser Tok
bracketP s = satisfyP (== TBracket s)

commaP ::  Parser Tok
commaP = symP ","

semicolonP :: Parser Tok
semicolonP = satisfyP (consP "SemiColon")

parenP :: Parser a -> Parser a
parenP = between (singleP (TBracket '(')) (singleP (TBracket ')'))

curlyP :: Parser a -> Parser a
curlyP = between (singleP (TBracket '{')) (singleP (TBracket '}'))

squareP :: Parser a -> Parser a
squareP = between (singleP (TBracket '[')) (singleP (TBracket ']'))

eofP :: Parser Tok
eofP = satisfyP (consP "EOF")

runParse p = runParserT p ""
