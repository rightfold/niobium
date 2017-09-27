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
import Niobiumc.Annotation (Position, PostParse)
import Niobiumc.Syntax (Declaration (..), ExecuteQueryResultAction (..), Expression (..), NamespaceName (..), Statement (..), Type (..), VariableName (..), expressionAnnotation)
import Niobiumc.Lex (Lexeme (..), Token (..))

import qualified Data.List.NonEmpty as NonEmpty
import qualified Text.Parsec as P



type Parser = P.Parsec [Lexeme] ()



namespaceName :: Parser (Position, NamespaceName)
namespaceName = do
  name@((position, _) :| _) <- identifier `sepBy1'` token Period
  pure (position, NamespaceName (snd <$> name))
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
  name <- identifier'
  _ <- token LeftParenthesis
  parameters <- ((,) <$> identifier' <*> type_) `P.sepBy` token Comma
  _ <- token RightParenthesis
  returnType <- type_
  _ <- token EqualsSign
  body <- expression
  pure $ FunctionDeclaration position name parameters returnType body

procedureDeclaration :: Parser (Declaration PostParse)
procedureDeclaration = do
  Lexeme position _ <- token ProcedureKeyword
  name <- identifier'
  using <- fmap fold . P.optionMaybe $ do
    _ <- token UsingKeyword
    parameters <- ((,) <$> identifier' <*> type_) `P.sepBy` token Comma
    pure parameters
  giving <- fmap fold . P.optionMaybe $ do
    _ <- token GivingKeyword
    parameters <- ((,) <$> identifier' <*> type_) `P.sepBy` token Comma
    pure parameters
  _ <- token BeginKeyword
  body <- P.many statement
  _ <- token EndKeyword
  pure $ ProcedureDeclaration position name using giving body



statement :: Parser (Statement PostParse)
statement = P.choice
  [ callStatement
  , executeQueryStatement
  , forEachStatement
  ]

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
    executeQueryResultAction =
      SingleRowExecuteQueryResultAction <$ token SingleRowKeyword

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



expression :: Parser (Expression PostParse)
expression = P.choice
  [ expression1
  , expression0
  ]

expression1 :: Parser (Expression PostParse)
expression1 = applyExpression expression0

expression0 :: Parser (Expression PostParse)
expression0 = variableExpression

variableExpression :: Parser (Expression PostParse)
variableExpression = do
  (position, name) <- variableName
  pure $ VariableExpression position Nothing name

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



type_ :: Parser (Type PostParse)
type_ = P.choice
  [ setType
  , textType
  , uuidType
  ]

setType :: Parser (Type PostParse)
setType = do
  Lexeme position _ <- token $ Identifier "set"
  _ <- token LeftParenthesis
  element <- type_
  _ <- token RightParenthesis
  pure $ SetType position element

textType :: Parser (Type PostParse)
textType = do
  Lexeme position _ <- token $ Identifier "text"
  pure $ TextType position

uuidType :: Parser (Type PostParse)
uuidType = do
  Lexeme position _ <- token $ Identifier "uuid"
  pure $ UUIDType position



token :: Token -> Parser Lexeme
token expected = P.try . (P.<?> show expected) $ do
  lexeme@(Lexeme _ actual) <- P.anyToken
  when (actual /= expected) $
    P.unexpected $ show actual
  pure lexeme

identifier :: Parser (Position, Text)
identifier = P.try . (P.<?> "Identifier") $ do
  Lexeme position actual <- P.anyToken
  case actual of
    Identifier name -> pure (position, name)
    _ -> P.unexpected $ show actual

identifier' :: Parser Text
identifier' = snd <$> identifier

stringLiteral :: Parser (Position, Text)
stringLiteral = P.try . (P.<?> "StringLiteral") $ do
  Lexeme position actual <- P.anyToken
  case actual of
    StringLiteral text -> pure (position, text)
    _ -> P.unexpected $ show actual

stringLiteral' :: Parser Text
stringLiteral' = snd <$> stringLiteral