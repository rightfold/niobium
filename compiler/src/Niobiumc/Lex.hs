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
import Data.Maybe (isJust)
import Data.Text (Text)
import Niobiumc.Syntax (Position)

import qualified Data.Text as Text
import qualified Text.Parsec as P
import qualified Text.Parsec.Text as P

data Lexeme = Lexeme Position Token
  deriving (Eq, Show)

data Token
  = Identifier Text

  | AddKeyword
  | AnonymousKeyword
  | ByKeyword
  | CallKeyword
  | DoKeyword
  | EndKeyword
  | ExecuteQueryKeyword
  | ForEachKeyword
  | FunctionKeyword
  | GivingKeyword
  | IgnoreRowsKeyword
  | ImplementationKeyword
  | InKeyword
  | IntKeyword
  | InterfaceKeyword
  | MultiplyKeyword
  | NamespaceKeyword
  | NullNamespaceKeyword
  | ProcedureKeyword
  | ReportHandlerKeyword
  | SingleRowKeyword
  | ToKeyword
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
  escape <- fmap isJust . P.optionMaybe . P.char $ '@'
  let head = P.oneOf $ ['A'..'Z'] ++ ['a'..'z']
  let tail = P.oneOf $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['_', '-']
  name <- fmap Text.pack $ (:) <$> head <*> P.many tail
  pure $ case (escape, Text.toUpper name) of
    (False, "ADD") -> AddKeyword
    (False, "ANONYMOUS") -> AnonymousKeyword
    (False, "BY") -> ByKeyword
    (False, "CALL") -> CallKeyword
    (False, "DO") -> DoKeyword
    (False, "END") -> EndKeyword
    (False, "EXECUTE-QUERY") -> ExecuteQueryKeyword
    (False, "FOR-EACH") -> ForEachKeyword
    (False, "FUNCTION") -> FunctionKeyword
    (False, "GIVING") -> GivingKeyword
    (False, "IGNORE-ROWS") -> IgnoreRowsKeyword
    (False, "IMPLEMENTATION") -> ImplementationKeyword
    (False, "IN") -> InKeyword
    (False, "INT") -> IntKeyword
    (False, "INTERFACE") -> InterfaceKeyword
    (False, "MULTIPLY") -> MultiplyKeyword
    (False, "NAMESPACE") -> NamespaceKeyword
    (False, "NULL-NAMESPACE") -> NullNamespaceKeyword
    (False, "PROCEDURE") -> ProcedureKeyword
    (False, "REPORT-HANDLER") -> ReportHandlerKeyword
    (False, "SINGLE-ROW") -> SingleRowKeyword
    (False, "TO") -> ToKeyword
    (False, "USING") -> UsingKeyword
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
