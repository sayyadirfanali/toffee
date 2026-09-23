module Main where

import Test.HUnit
import Text.Megaparsec.Pos
import Data.Either (isRight, isLeft)

import qualified LexerTests as L
import qualified LayoutTests as Y
import System.Exit

main :: IO Counts
main = do
  c <- runTestTT (TestList [TestLabel "lexer" L.tests, TestLabel "layout" Y.tests])
  if errors c + failures c > 0 then exitFailure else exitSuccess