module Main where

import Test.HUnit

import Control.Monad
import Control.Monad.State
import Data.Either
import System.IO

import Text.Megaparsec
import Test.HUnit.Base

import Lexer
import Parser

type LexRes = Either (ParseErrorBundle String String) [TokInfo]

equalLex :: String -> LexRes -> Test
equalLex s e = runLex >>= (@?= e)

boolLex :: String -> (LexRes -> Bool) -> Test
boolLex s p = runLex >>= \a -> p a @? ""

null = TestList
  [
    equalLex "null" (Right (TLit (LNull "null"))),
    boolLex  "nil"  (\e -> isLeft e)
  ]

main = runTestTT t
