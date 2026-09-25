module Main where

import qualified Expr as E
import qualified Lexer as L
import qualified Layout as Y
import qualified Parser as P
import Data.Void
import System.Environment
import Text.Megaparsec

main :: IO ()
main = do
  args <- getArgs
  let file = case args of
        (f : _) -> f
        []      -> "example.txt"
  s <- readFile file

  case L.runLex s of
    Left lexErr -> putStr (errorBundlePretty lexErr)
    Right lexVal -> do
      section "lexer"
      mapM_ (print . L.tokVal) lexVal
      section "preLay"
      mapM_ (print . L.tokVal) (Y.preLay lexVal)
      case Y.layout lexVal of
        Left layErr -> print layErr
        Right layVal -> do
          section "layout"
          mapM_ (print . L.tokVal) layVal
          section "parser"
          result <- P.runParse topP layVal
          case result of
            Left bundle -> mapM_ (report layVal) (bundleErrors bundle)
            Right exprs -> mapM_ print exprs

section :: String -> IO ()
section name = putStrLn ("--- " ++ name ++ " ---")

topP :: P.Parser [E.Expr]
topP = P.bracketP '<' *> sepEndBy E.exprE P.semicolonP <* P.bracketP '>' <* P.eofP

report :: [L.TokInfo] -> ParseError [L.TokInfo] Void -> IO ()
report toks err = do
  let i   = min (errorOffset err) (length toks - 1)
      pos = L.tokStart (toks !! i)
  putStr (sourcePosPretty pos ++ ": " ++ parseErrorTextPretty err)