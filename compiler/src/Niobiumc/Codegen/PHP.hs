module Niobiumc.Codegen.PHP
  ( ObjectgenT
  , Objectgen
  , runObjectgenT
  , runObjectgen

  , CodegenT
  , Codegen
  , runCodegenT
  , runCodegen

  , objectgenDeclaration
  , codegenStatement
  , codegenExpression
  ) where

import Control.Monad.RWS (RWST, runRWST)
import Data.ByteString.Builder (Builder)
import Data.Foldable (for_, traverse_)
import Data.Functor.Identity (Identity (..))
import Data.Semigroup ((<>))
import Data.Text.Encoding (encodeUtf8)
import Niobiumc.Syntax (Declaration (..), Expression (..), NamespaceName (..), PostCheck, Statement (..), Type (..), VariableName (..))

import qualified Control.Monad.Writer as Writer
import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text



type ObjectgenT = RWST () Builder ()

type Objectgen = ObjectgenT Identity

runObjectgenT :: Functor m => ObjectgenT m a -> m (a, Builder)
runObjectgenT action = do { (a, _, w) <- runRWST action () ()
                          ; pure (a, w) }

runObjectgen :: Objectgen a -> (a, Builder)
runObjectgen = runIdentity . runObjectgenT



type CodegenT = RWST () Builder ()

type Codegen = CodegenT Identity

runCodegenT :: Monad m => CodegenT m a -> m (a, Builder)
runCodegenT action = do { (a, _, bs) <- runRWST action () ()
                        ; pure (a, bs) }

runCodegen :: Codegen a -> (a, Builder)
runCodegen = runIdentity . runCodegenT



objectgenDeclaration :: Monad m => Declaration PostCheck -> ObjectgenT m ()
objectgenDeclaration (NamespaceDeclaration _ _) = pure ()
objectgenDeclaration (UsingDeclaration _ _) = pure ()
objectgenDeclaration (FunctionDeclaration (_, ns) name parameters returnType body) =
  objectgenFunction ns name parameters returnType body
objectgenDeclaration (ProcedureDeclaration (_, ns) name using giving body) =
  objectgenProcedure ns name using giving body

codegenStatement :: Monad m => Statement PostCheck -> CodegenT (ObjectgenT m) ()
codegenStatement (AddStatement _ left right giving) = do
  left' <- codegenExpression left
  right' <- codegenExpression right
  let giving' = codegenLocalName giving
  Writer.tell $ giving' <> " = niobium\\language\\add_int(" <> left' <> ", " <> right' <> ");\n"
codegenStatement (CallStatement _ callee using giving) = do
  callee' <- codegenExpression callee
  using' <- traverse codegenExpression using
  let giving' = fmap codegenLocalName giving
  let arguments = using' <> fmap ("&" <>) giving'
  Writer.tell $ callee' <> "($context" <> foldMap (", " <>) arguments <> ");\n"
codegenStatement (MultiplyStatement _ left right giving) = do
  left' <- codegenExpression left
  right' <- codegenExpression right
  let giving' = codegenLocalName giving
  Writer.tell $ giving' <> " = niobium\\language\\multiply_int(" <> left' <> ", " <> right' <> ");\n"

codegenExpression :: Monad m => Expression PostCheck -> CodegenT (ObjectgenT m) Builder
codegenExpression (ReportHandlerExpression _ subroutine) =
  pure "TODO"
codegenExpression (VariableExpression _ (Just ns) name) =
  pure $ "'" <> codegenGlobalName ns name <> "'"
codegenExpression (VariableExpression _ Nothing name) =
  pure $ codegenLocalName name



objectgenFunction
  :: Monad m
  => NamespaceName
  -> VariableName
  -> [(VariableName, Type PostCheck)]
  -> Type PostCheck
  -> Expression PostCheck
  -> ObjectgenT m ()
objectgenFunction ns name parameters returnType body = do
  (result, body') <- runCodegenT $ codegenExpression body
  Writer.tell $ "function " <> codegenGlobalName ns name <> "("
  Writer.tell $ ") {\n"
  Writer.tell $ body'
  Writer.tell $ "return " <> result <> ";\n"
  Writer.tell $ "}\n"

objectgenProcedure
  :: Monad m
  => NamespaceName
  -> VariableName
  -> [(VariableName, Type PostCheck)]
  -> [(VariableName, Type PostCheck)]
  -> [Statement PostCheck]
  -> ObjectgenT m ()
objectgenProcedure ns name using giving body = do
  ((), body') <- runCodegenT $ traverse_ codegenStatement body
  Writer.tell $ "function " <> codegenGlobalName ns name <> "($context"
  for_ using $ \(pname, _) -> Writer.tell $ ", " <> codegenLocalName pname
  for_ giving $ \(pname, _) -> Writer.tell $ ", " <> "&" <> codegenGivingName pname
  Writer.tell $ ") {\n"
  Writer.tell $ body'
  for_ giving $ \(pname, _) -> Writer.tell $ codegenGivingName pname <> " = " <> codegenLocalName pname <> ";\n"
  Writer.tell $ "}\n"



codegenGlobalName :: NamespaceName -> VariableName -> Builder
codegenGlobalName (NamespaceName ns) (VariableName name) =
  Builder.byteString . encodeUtf8 $
    Text.intercalate "__" (fmap (Text.replace "-" "_") (ns <> [name]))

codegenLocalName :: VariableName -> Builder
codegenLocalName (VariableName name) =
  Builder.byteString . encodeUtf8 $ "$local_" <> Text.replace "-" "_" name

codegenGivingName :: VariableName -> Builder
codegenGivingName (VariableName name) =
  Builder.byteString . encodeUtf8 $ "$giving_" <> Text.replace "-" "_" name
