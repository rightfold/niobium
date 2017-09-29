module Niobiumc.Lex
  ( Lexeme (..)
  , Token (..)
  , lexeme
  , token
  , whitespace
  ) where

import Prelude hiding (head, tail)

import Control.Monad (when)
import Data.Functor (void)
import Data.Text (Text)
import Niobiumc.Annotation (Position)

import qualified Data.Text as Text
import qualified Text.Parsec as P
import qualified Text.Parsec.Text as P

data Lexeme = Lexeme Position Token
  deriving (Eq, Show)

data Token
  = Identifier Text

  | BeginKeyword
  | CallKeyword
  | DoKeyword
  | EndKeyword
  | ExecuteQueryKeyword
  | ForEachKeyword
  | FunctionKeyword
  | GivingKeyword
  | IgnoreRowsKeyword
  | InKeyword
  | NamespaceKeyword
  | NullNamespaceKeyword
  | ProcedureKeyword
  | SingleRowKeyword
  | UsingKeyword

  | Comma
  | EqualsSign
  | LeftParenthesis
  | Period
  | RightParenthesis

  | StringLiteral Text
  deriving (Eq, Show)

lexeme :: P.Parser Lexeme
lexeme = do
  position <- P.statePos <$> P.getParserState
  token' <- token
  _ <- whitespace
  pure $ Lexeme position token'

token :: P.Parser Token
token = P.choice [ identifierOrKeyword

                 , comma
                 , equalsSign
                 , leftParenthesis
                 , period
                 , rightParenthesis

                 , stringLiteral
                 ]

identifierOrKeyword :: P.Parser Token
identifierOrKeyword = do
  let head = P.oneOf $ ['A'..'Z'] ++ ['a'..'z']
  let tail = P.oneOf $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['_', '-']
  name <- fmap Text.pack $ (:) <$> head <*> P.many tail
  pure $ case Text.toUpper name of
    "BEGIN" -> BeginKeyword
    "CALL" -> CallKeyword
    "DO" -> DoKeyword
    "END" -> EndKeyword
    "EXECUTE-QUERY" -> ExecuteQueryKeyword
    "FOR-EACH" -> ForEachKeyword
    "FUNCTION" -> FunctionKeyword
    "GIVING" -> GivingKeyword
    "IGNORE-ROWS" -> IgnoreRowsKeyword
    "IN" -> InKeyword
    "NAMESPACE" -> NamespaceKeyword
    "NULL-NAMESPACE" -> NullNamespaceKeyword
    "PROCEDURE" -> ProcedureKeyword
    "SINGLE-ROW" -> SingleRowKeyword
    "USING" -> UsingKeyword
    _ -> Identifier name

comma :: P.Parser Token
comma = Comma <$ P.char ','

equalsSign :: P.Parser Token
equalsSign = EqualsSign <$ P.char '='

leftParenthesis :: P.Parser Token
leftParenthesis = LeftParenthesis <$ P.char '('

period :: P.Parser Token
period = Period <$ P.char '.'

rightParenthesis :: P.Parser Token
rightParenthesis = RightParenthesis <$ P.char ')'

stringLiteral :: P.Parser Token
stringLiteral = do
  _ <- P.char '"'
  text <- fmap Text.pack . P.many . P.noneOf $ ['"']
  _ <- P.char '"'
  pure $ StringLiteral text

whitespace :: P.Parser ()
whitespace = void $ P.many (void space P.<|> P.try comment)
  where space = P.space
        comment = do
          _ <- P.oneOf ['N', 'n']
          _ <- P.oneOf ['B', 'b']
          s <- space
          _ <- when (s /= '\n') (void $ P.many (P.noneOf ['\n']))
          pure ()
