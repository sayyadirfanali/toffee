module Main where

import Data.Void (Void)
import System.Environment (getArgs)
import Text.Megaparsec (ParseError, bundleErrors, errorBundlePretty, errorOffset, parseErrorTextPretty, sourcePosPretty)

import Expr (programP)
import Layout (layout, preLay)
import Lexer (TokInfo (tokStart, tokVal), runLex)
import Parser (runParse)

main :: IO ()
main = do
  args <- getArgs
  let file = case args of
        (f : _) -> f
        []      -> "example.txt"
  s <- readFile file

  case runLex s of
    Left lexErr -> putStr (errorBundlePretty lexErr)
    Right lexVal -> do
      section "lexer"
      mapM_ (print . tokVal) lexVal
      section "preLay"
      mapM_ (print . tokVal) (preLay lexVal)
      case layout lexVal of
        Left layErr -> print layErr
        Right layVal -> do
          section "layout"
          mapM_ (print . tokVal) layVal
          section "parser"
          case runParse programP layVal of
            Left bundle -> mapM_ (report layVal) (bundleErrors bundle)
            Right exprs -> mapM_ print exprs

section :: String -> IO ()
section name = putStrLn ("--- " ++ name ++ " ---")

report :: [TokInfo] -> ParseError [TokInfo] Void -> IO ()
report toks err = do
  let i   = min (errorOffset err) (length toks - 1)
      pos = tokStart (toks !! i)
  putStr (sourcePosPretty pos ++ ": " ++ parseErrorTextPretty err)
