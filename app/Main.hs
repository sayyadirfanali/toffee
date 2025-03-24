module Main where

import Control.Monad
import System.Environment
import System.IO
import Text.Megaparsec

import qualified Lexer  as L
import qualified Parser as P
import qualified Expr   as E
import qualified Stmnt  as S

_main = do
  c : _ <- getArgs
  s     <- readFile "example.txt"
  x     <- L.runLex s
  case x of
    Left e  -> putStrLn (errorBundlePretty e)
    Right v -> do
      case c of
        "-l" -> mapM_ print (fmap L.tokVal v)
        "-p" -> do
          mapM_ putStrLn ["", "***", ""]
          y <- P.runParse S.stmnt v
          case y of
            Left e  -> do
              print (bundleErrors e)
              let es = bundleErrors e
              forM_ es $ \e -> case e of { TrivialError _ (Just (Tokens t)) _ -> print t }
            Right v -> print v

main = do
  x <- L.runLex "null"
  case x of
    Left e  -> putStrLn (errorBundlePretty e)
    Right v -> mapM_ print (fmap L.tokVal v)
