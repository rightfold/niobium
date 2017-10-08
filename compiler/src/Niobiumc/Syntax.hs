{-# LANGUAGE CPP #-}

module Niobiumc.Syntax where

import Data.Text (Text)
import Text.Parsec (SourcePos)



newtype NamespaceName = NamespaceName [Text] deriving (Eq, Ord, Show)
newtype VariableName = VariableName Text deriving (Eq, Ord, Show)



data Declaration s
  = NamespaceDeclaration (DeclarationAnnotation s) NamespaceName
  | UsingDeclaration (DeclarationAnnotation s) NamespaceName
  | FunctionDeclaration (DeclarationAnnotation s) VariableName [(VariableName, Type s)] (Type s) (Expression s)
  | ProcedureDeclaration (DeclarationAnnotation s) VariableName [(VariableName, Type s)] [(VariableName, Type s)] [Statement s]

declarationAnnotation :: Declaration s -> DeclarationAnnotation s
declarationAnnotation (NamespaceDeclaration a _) = a
declarationAnnotation (UsingDeclaration a _) = a
declarationAnnotation (FunctionDeclaration a _ _ _ _) = a
declarationAnnotation (ProcedureDeclaration a _ _ _ _) = a



data Statement s
  = AddStatement (StatementAnnotation s) (Expression s) (Expression s) VariableName
  | CallStatement (StatementAnnotation s) (Expression s) [Expression s] [VariableName]
  | ExecuteQueryStatement (StatementAnnotation s) Text [Expression s] [VariableName] ExecuteQueryResultAction
  | ForEachStatement (StatementAnnotation s) VariableName (Expression s) [Statement s]
  | MultiplyStatement (StatementAnnotation s) (Expression s) (Expression s) VariableName

statementAnnotation :: Statement s -> StatementAnnotation s
statementAnnotation (AddStatement a _ _ _) = a
statementAnnotation (CallStatement a _ _ _) = a
statementAnnotation (ExecuteQueryStatement a _ _ _ _) = a
statementAnnotation (ForEachStatement a _ _ _) = a
statementAnnotation (MultiplyStatement a _ _ _) = a

data ExecuteQueryResultAction
  = IgnoreRowsExecuteQueryResultAction
  | SingleRowExecuteQueryResultAction
  deriving (Eq, Show)



data Expression s
  = ApplyExpression (ExpressionAnnotation s) (Expression s) [Expression s]
  | ReportExpression (ExpressionAnnotation s) (Expression s)
  | VariableExpression (ExpressionAnnotation s) (Maybe NamespaceName) VariableName

expressionAnnotation :: Expression s -> ExpressionAnnotation s
expressionAnnotation (ApplyExpression a _ _) = a
expressionAnnotation (ReportExpression a _) = a
expressionAnnotation (VariableExpression a _ _) = a

typeOf :: Expression PostCheck -> Type PostCheck
typeOf = snd . expressionAnnotation



data Type s
  = FunctionType (TypeAnnotation s) [Type s] (Type s)
  | IntType (TypeAnnotation s)
  | ProcedureType (TypeAnnotation s) [Type s] [Type s]
  | ReportType (TypeAnnotation s)

typeAnnotation :: Type s -> TypeAnnotation s
typeAnnotation (FunctionType a _ _) = a
typeAnnotation (IntType a) = a
typeAnnotation (ProcedureType a _ _) = a
typeAnnotation (ReportType a) = a



type Position = SourcePos

type family DeclarationAnnotation s :: *
type family StatementAnnotation s :: *
type family ExpressionAnnotation s :: *
type family TypeAnnotation s :: *

data PostParse
type instance DeclarationAnnotation PostParse = Position
type instance StatementAnnotation PostParse = Position
type instance ExpressionAnnotation PostParse = Position
type instance TypeAnnotation PostParse = Position

data PostCheck
type instance DeclarationAnnotation PostCheck = (Position, NamespaceName)
type instance StatementAnnotation PostCheck = Position
type instance ExpressionAnnotation PostCheck = (Position, Type PostCheck)
type instance TypeAnnotation PostCheck = Position



#define NB_SYNTAX_INSTANCES(t)                                                \
  deriving instance Eq (t PostParse)                                         ;\
  deriving instance Eq (t PostCheck)                                         ;\
  deriving instance Show (t PostParse)                                       ;\
  deriving instance Show (t PostCheck)
NB_SYNTAX_INSTANCES(Declaration)
NB_SYNTAX_INSTANCES(Statement)
NB_SYNTAX_INSTANCES(Expression)
NB_SYNTAX_INSTANCES(Type)
#undef NB_SYNTAX_INSTANCES
