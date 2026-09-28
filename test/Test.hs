module Main where

import Test.HUnit

import qualified LexerTests as L
import qualified LayoutTests as Y
import qualified ExprTests as E
import System.Exit

main :: IO Counts
main = do
  c <- runTestTT $
    TestList [
        TestLabel "lexer" L.tests
      , TestLabel "layout" Y.tests
      , TestLabel "expr" E.tests
      ]
  if errors c + failures c > 0 then exitFailure else exitSuccess
