module Niobiumc.Parse
  ( Parser
  , declaration
  , statement
  , expression
  , type_
  ) where

import Control.Monad (when)
import Data.Foldable (fold)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Niobiumc.Lex (Lexeme (..), Token (..))
import Niobiumc.Syntax (Declaration (..), ExecuteQueryResultAction (..), Expression (..), NamespaceName (..), Position, PostParse, Statement (..), Type (..), VariableName (..), expressionAnnotation)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Text.Parsec as P



type Parser = P.Parsec [Lexeme] ()



namespaceName :: Parser (Position, NamespaceName)
namespaceName = nullNamespaceName P.<|> nonNullNamespaceName
  where
    nullNamespaceName :: Parser (Position, NamespaceName)
    nullNamespaceName = do
      Lexeme position _ <- token NullNamespaceKeyword
      pure (position, NamespaceName [])

    nonNullNamespaceName :: Parser (Position, NamespaceName)
    nonNullNamespaceName = do
      name@((position, _) :| _) <- identifier `sepBy1'` token Period
      pure (position, NamespaceName (snd <$> NonEmpty.toList name))
      where p `sepBy1'` s = NonEmpty.fromList <$> p `P.sepBy1` s

namespaceName' :: Parser NamespaceName
namespaceName' = snd <$> namespaceName

variableName :: Parser (Position, VariableName)
variableName = fmap VariableName <$> identifier

variableName' :: Parser VariableName
variableName' = snd <$> variableName



declaration :: Parser (Declaration PostParse)
declaration = P.choice
  [ namespaceDeclaration
  , usingDeclaration
  , functionDeclaration
  , procedureDeclaration
  ]

namespaceDeclaration :: Parser (Declaration PostParse)
namespaceDeclaration = do
  Lexeme position _ <- token NamespaceKeyword
  name <- namespaceName'
  pure $ NamespaceDeclaration position name

usingDeclaration :: Parser (Declaration PostParse)
usingDeclaration = do
  Lexeme position _ <- token UsingKeyword
  name <- namespaceName'
  pure $ UsingDeclaration position name

functionDeclaration :: Parser (Declaration PostParse)
functionDeclaration = do
  Lexeme position _ <- token FunctionKeyword
  name <- variableName'
  _ <- token LeftParenthesis
  parameters <- ((,) <$> variableName' <*> type_) `P.sepBy` token Comma
  _ <- token RightParenthesis
  returnType <- type_
  _ <- token EqualsSign
  body <- expression
  pure $ FunctionDeclaration position name parameters returnType body

procedureDeclaration :: Parser (Declaration PostParse)
procedureDeclaration = do
  Lexeme position _ <- token ProcedureKeyword
  name <- variableName'
  _ <- token InterfaceKeyword
  using <- fmap fold . P.optionMaybe $ do
    _ <- token UsingKeyword
    parameters <- ((,) <$> variableName' <*> type_) `P.sepBy` token Comma
    pure parameters
  giving <- fmap fold . P.optionMaybe $ do
    _ <- token GivingKeyword
    parameters <- ((,) <$> variableName' <*> type_) `P.sepBy` token Comma
    pure parameters
  _ <- token ImplementationKeyword
  body <- P.many statement
  _ <- token EndKeyword
  pure $ ProcedureDeclaration position name using giving body



statement :: Parser (Statement PostParse)
statement = P.choice
  [ addStatement
  , callStatement
  , executeQueryStatement
  , forEachStatement
  , multiplyStatement
  ]

addStatement :: Parser (Statement PostParse)
addStatement = do
  Lexeme position _ <- token AddKeyword
  x <- expression
  _ <- token ToKeyword
  y <- expression
  _ <- token GivingKeyword
  z <- variableName'
  pure $ AddStatement position x y z

callStatement :: Parser (Statement PostParse)
callStatement = do
  Lexeme position _ <- token CallKeyword
  procedure <- expression
  using <- fmap fold . P.optionMaybe $ do
    _ <- token UsingKeyword
    arguments <- expression `P.sepBy` token Comma
    pure arguments
  giving <- fmap fold . P.optionMaybe $ do
    _ <- token GivingKeyword
    arguments <- variableName' `P.sepBy` token Comma
    pure arguments
  pure $ CallStatement position procedure using giving

executeQueryStatement :: Parser (Statement PostParse)
executeQueryStatement = do
  Lexeme position _ <- token ExecuteQueryKeyword
  query <- stringLiteral'
  using <- fmap fold . P.optionMaybe $ do
    _ <- token UsingKeyword
    arguments <- expression `P.sepBy` token Comma
    pure arguments
  giving <- fmap fold . P.optionMaybe $ do
    _ <- token GivingKeyword
    arguments <- variableName' `P.sepBy` token Comma
    pure arguments
  resultAction <- executeQueryResultAction
  pure $ ExecuteQueryStatement position query using giving resultAction
  where
    executeQueryResultAction :: Parser ExecuteQueryResultAction
    executeQueryResultAction = P.choice
      [ IgnoreRowsExecuteQueryResultAction <$ token IgnoreRowsKeyword
      , SingleRowExecuteQueryResultAction <$ token SingleRowKeyword
      ]

forEachStatement :: Parser (Statement PostParse)
forEachStatement = do
  Lexeme position _ <- token ForEachKeyword
  name <- variableName'
  _ <- token InKeyword
  iterable <- expression
  _ <- token DoKeyword
  body <- P.many statement
  _ <- token EndKeyword
  pure $ ForEachStatement position name iterable body

multiplyStatement :: Parser (Statement PostParse)
multiplyStatement = do
  Lexeme position _ <- token MultiplyKeyword
  x <- expression
  _ <- token ByKeyword
  y <- expression
  _ <- token GivingKeyword
  z <- variableName'
  pure $ MultiplyStatement position x y z



expression :: Parser (Expression PostParse)
expression = P.choice
  [ expression1
  , expression0
  ]

expression1 :: Parser (Expression PostParse)
expression1 = applyExpression expression0

expression0 :: Parser (Expression PostParse)
expression0 = P.choice
  [ variableExpression
  , reportHandlerExpression expression
  ]

applyExpression :: Parser (Expression PostParse) -> Parser (Expression PostParse)
applyExpression next = do
  function <- next
  let position = expressionAnnotation function
  argumentLists <- P.many $ do
    _ <- token LeftParenthesis
    arguments <- expression `P.sepBy` token Comma
    _ <- token RightParenthesis
    pure arguments
  pure $ foldl' (ApplyExpression position) function argumentLists

reportHandlerExpression :: Parser (Expression PostParse) -> Parser (Expression PostParse)
reportHandlerExpression next = do
  Lexeme position _ <- token ReportHandlerKeyword
  subroutine <- next
  pure $ ReportHandlerExpression position subroutine

variableExpression :: Parser (Expression PostParse)
variableExpression = do
  (position, name) <- variableName
  pure $ VariableExpression position Nothing name



type_ :: Parser (Type PostParse)
type_ = P.choice
  [ intType
  , reportHandlerType
  ]

intType :: Parser (Type PostParse)
intType = do
  Lexeme position _ <- token IntKeyword
  pure $ IntType position

reportHandlerType :: Parser (Type PostParse)
reportHandlerType = do
  Lexeme position _ <- token ReportHandlerKeyword
  pure $ ReportHandlerType position



anyToken :: Parser Lexeme
anyToken = do
  lexeme@(Lexeme position _) <- P.anyToken
  _ <- P.setPosition position
  pure lexeme

token :: Token -> Parser Lexeme
token expected = (P.<?> show expected) . P.try $ do
  lexeme@(Lexeme _ actual) <- anyToken
  when (actual /= expected) $
    P.unexpected $ show actual
  pure lexeme

identifier :: Parser (Position, Text)
identifier = (P.<?> "Identifier") . P.try $ do
  Lexeme position actual <- anyToken
  case actual of
    Identifier name -> pure (position, name)
    _ -> P.unexpected $ show actual

stringLiteral :: Parser (Position, Text)
stringLiteral = (P.<?> "StringLiteral") . P.try $ do
  Lexeme position actual <- anyToken
  case actual of
    StringLiteral text -> pure (position, text)
    _ -> P.unexpected $ show actual

stringLiteral' :: Parser Text
stringLiteral' = snd <$> stringLiteral
