module Niobiumc.Syntax where

import Data.Text (Text)
import Niobiumc.Annotation (DeclarationAnnotation, ExpressionAnnotation, StatementAnnotation, TypeAnnotation)



newtype NamespaceName = NamespaceName [Text] deriving (Eq, Ord, Show)
newtype VariableName = VariableName [Text] deriving (Eq, Ord, Show)



data Declaration s
  = NamespaceDeclaration (DeclarationAnnotation s) NamespaceName
  | UsingDeclaration (DeclarationAnnotation s) NamespaceName
  | FunctionDeclaration (DeclarationAnnotation s) Text [(Text, Type s)] (Type s) (Expression s)
  | ProcedureDeclaration (DeclarationAnnotation s) Text [(Text, Type s)] [(Text, Type s)] [Statement s]

declarationAnnotation :: Declaration s -> DeclarationAnnotation s
declarationAnnotation (NamespaceDeclaration a _) = a
declarationAnnotation (UsingDeclaration a _) = a
declarationAnnotation (FunctionDeclaration a _ _ _ _) = a
declarationAnnotation (ProcedureDeclaration a _ _ _ _) = a



data Statement s
  = CallStatement (StatementAnnotation s) (Expression s) [Expression s] [VariableName]
  | ExecuteQueryStatement (StatementAnnotation s) Text [Expression s] [VariableName] ExecuteQueryResultAction
  | ForEachStatement (StatementAnnotation s) VariableName (Expression s) [Statement s]

statementAnnotation :: Statement s -> StatementAnnotation s
statementAnnotation (CallStatement a _ _ _) = a
statementAnnotation (ExecuteQueryStatement a _ _ _ _) = a
statementAnnotation (ForEachStatement a _ _ _) = a

data ExecuteQueryResultAction
  = SingleRowExecuteQueryResultAction



data Expression s
  = VariableExpression (ExpressionAnnotation s) (Maybe NamespaceName) VariableName
  | ApplyExpression (ExpressionAnnotation s) (Expression s) [Expression s]

expressionAnnotation :: Expression s -> ExpressionAnnotation s
expressionAnnotation (VariableExpression a _ _) = a
expressionAnnotation (ApplyExpression a _ _) = a



data Type s
  = SetType (TypeAnnotation s) (Type s)
  | TextType (TypeAnnotation s)
  | UUIDType (TypeAnnotation s)

typeAnnotation :: Type s -> TypeAnnotation s
typeAnnotation (SetType a _) = a
typeAnnotation (TextType a) = a
typeAnnotation (UUIDType a) = a
