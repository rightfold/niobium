module Main
  ( main
  ) where

import Data.Foldable (traverse_)
import Niobiumc.Check (checkDeclaration, runCheck)
import Niobiumc.Codegen.C (codegenDeclaration, codegenFooter, codegenHeader, runCodegen)
import Niobiumc.Lex (lexeme, whitespace)
import Niobiumc.Parse (declaration)
import System.Environment (getArgs)
import System.IO (stdout)

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text.IO as Text.IO
import qualified Text.Parsec as P

main :: IO ()
main = do
  [filename] <- getArgs
  text <- Text.IO.readFile filename

  lexemes <- case P.parse (whitespace *> P.many lexeme <* P.eof) "" text of
    Left err -> fail (show err)
    Right ok -> pure ok
  declarationsPostParse <- case P.parse (P.many declaration <* P.eof) "" lexemes of
    Left err -> fail (show err)
    Right ok -> pure ok
  print declarationsPostParse

  declarationsPostCheck <- case runCheck (traverse checkDeclaration declarationsPostParse) of
    Left err -> fail (show err)
    Right ok -> pure ok
  print declarationsPostCheck

  Builder.hPutBuilder stdout . snd . runCodegen $ do
    codegenHeader
    traverse_ codegenDeclaration declarationsPostCheck
    codegenFooter
