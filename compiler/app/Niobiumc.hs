module Main
  ( main
  ) where

import Data.Foldable (for_, traverse_)
import Data.List (intercalate)
import Data.Semigroup ((<>))
import Niobiumc.Check (checkDeclaration, runCheck)
import Niobiumc.Codegen.Object (objectgenDeclaration, objectName, runObjectgen, write)
import Niobiumc.Lex (lexeme, whitespace)
import Niobiumc.Parse (declaration)
import Niobiumc.Syntax (NamespaceName (..), VariableName (..))
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.IO (IOMode (..), withFile)

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Text.Parsec as P

main :: IO ()
main = do
  [sourceFile, outputDirectory] <- getArgs
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

  let objects = snd . runObjectgen $
        traverse_ objectgenDeclaration declarationsPostCheck
  for_ objects $ \object -> do
    let (NamespaceName ns, VariableName name) = objectName object
        directory = intercalate "/" $ [outputDirectory] <> (Text.unpack <$> ns)
        filename = directory <> "/" <> Text.unpack name <> ".nbob"
    createDirectoryIfMissing True directory
    withFile filename WriteMode $ \file ->
      Builder.hPutBuilder file . write $ object
