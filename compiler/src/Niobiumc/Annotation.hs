module Niobiumc.Annotation where

import Text.Parsec (SourcePos)

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
