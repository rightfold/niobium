module Main
  ( main
  ) where

import Data.Foldable (traverse_)
import Niobiumc.Check (checkDeclaration, runCheck)
import Niobiumc.Codegen.PHP (objectgenDeclaration, runObjectgen)
import Niobiumc.Lex (lexeme, whitespace)
import Niobiumc.Parse (declaration)
import System.Environment (getArgs)
import System.IO (stdout)

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text.IO as Text.IO
import qualified Text.Parsec as P

main :: IO ()
main = do
  [sourceFile] <- getArgs
  text <- Text.IO.readFile sourceFile

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

  let ((), php) = runObjectgen $ traverse_ objectgenDeclaration declarationsPostCheck
  Builder.hPutBuilder stdout php
