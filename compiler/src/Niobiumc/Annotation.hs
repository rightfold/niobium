module Niobiumc.Annotation where

import Text.Parsec (SourcePos)

type Position = SourcePos

type family DeclarationAnnotation s :: *
type family StatementAnnotation s :: *
type family ExpressionAnnotation s :: *
type family TypeAnnotation s :: *
