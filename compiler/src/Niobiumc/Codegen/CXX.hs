{-# LANGUAGE TemplateHaskell #-}

module Niobiumc.Codegen.CXX
  ( Codegen
  , CodegenState (..)
  , runCodegen

  , codegenHeader
  , codegenFooter

  , codegenDeclaration
  , codegenStatement
  , codegenExpression
  ) where

import Control.Lens ((.=), (<<+=), makeLenses, use)
import Control.Monad.RWS (RWS, runRWS, tell)
import Data.ByteString.Builder (Builder)
import Data.Foldable (fold, foldl', traverse_)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Niobiumc.Syntax (Declaration (..), ExecuteQueryResultAction (..), Expression (..), NamespaceName (..), PostCheck, Statement (..), VariableName (..))

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder



type Codegen = RWS () Builder CodegenState

data CodegenState = CodegenState
  { _csNextTemporary :: Word
  , _csNextQueryResult :: Word
  , _csCurrentNamespace :: NamespaceName
  }

$(makeLenses ''CodegenState)

runCodegen :: Codegen a -> (a, Builder)
runCodegen action = let (a, _, c) = runRWS action () state in (a, c)
  where state = CodegenState { _csNextTemporary = 0
                             , _csNextQueryResult = 0
                             , _csCurrentNamespace = NamespaceName []
                             }



codegenHeader :: Codegen ()
codegenHeader = do
  tell $ "#include <niobium/context.hpp>\n"
  tell $ "#include <niobium/execution_log.hpp>\n"
  tell $ "#include <niobium/evaluation.hpp>\n"
  tell $ "#include <niobium/query.hpp>\n"
  tell $ "#include <niobium/value.hpp>\n"

codegenFooter :: Codegen ()
codegenFooter = pure ()



codegenDeclaration :: Declaration PostCheck -> Codegen ()
codegenDeclaration (NamespaceDeclaration _ ns) = csCurrentNamespace .= ns
codegenDeclaration (UsingDeclaration _ _) = pure ()
codegenDeclaration (FunctionDeclaration _ name parameters _ body) = do
  currentNS <- use csCurrentNamespace
  let parameters' = [ "nb::value " <> newLocal p | (p, _) <- parameters ]
  tell $ "extern \"C\" nb::value " <> newGlobal currentNS name
  tell $ "(" <> intercalate ", " parameters' <> ") {\n"
  result <- codegenExpression body
  tell $ "return " <> result <> ";\n"
  tell $ "}\n"
codegenDeclaration (ProcedureDeclaration _ name using giving body) = do
  currentNS <- use csCurrentNamespace
  let using' = [ "nb::value " <> newLocal u | (u, _) <- using ]
      giving' = [ "nb::value& " <> newLocal g | (g, _) <- giving ]
      context = "nb::context& context"
  tell $ "extern \"C\" void " <> newGlobal currentNS name
  tell $ "(" <> intercalate ", " (context : using' <> giving') <> ") {\n"
  tell $ "nb::execution_log::enter(context, " <> stringLiteral (fullName currentNS name) <> ");\n"
  traverse_ codegenStatement body
  tell $ "nb::execution_log::leave(context);\n"
  tell $ "}\n"

codegenStatement :: Statement PostCheck -> Codegen ()
codegenStatement (AddStatement _ x y z) = do
  x' <- codegenExpression x
  y' <- codegenExpression y
  z' <- getLocal z
  tell $ "NB_ADD(" <> intercalate ", " [z', x', y'] <> ");\n"
codegenStatement (CallStatement _ callee using giving) = do
  callee' <- codegenExpression callee
  using' <- traverse codegenExpression using
  giving' <- sequence [ getLocal g | g <- giving ]
  tell $ "NB_CALL(" <> intercalate ", " (callee' : using' <> giving') <> ");\n"
codegenStatement (ExecuteQueryStatement _ query using giving resultAction) = do
  let query' = stringLiteral query
      queryLength' = string . show . lengthUTF8 $ query
  using' <- traverse codegenExpression using
  giving' <- sequence [ ("&" <>) <$> getLocal g | g <- giving ]
  result <- newQueryResult
  tell $ "NB_EXECUTE_QUERY(" <> intercalate ", " (result : query' : queryLength' : using') <> ");\n"
  case resultAction of
    IgnoreRowsExecuteQueryResultAction -> pure ()
    SingleRowExecuteQueryResultAction ->
      tell $ "NB_EXECUTE_QUERY_NEXT(" <> intercalate ", " (result : giving') <> ");\n"
codegenStatement (ForEachStatement _ name iterable body) = do
  name' <- getLocal name
  iterable' <- codegenExpression iterable
  tell $ "NB_FOR_EACH(" <> name' <> ", " <> iterable' <> ", {\n"
  traverse_ codegenStatement body
  tell $ "});\n"
codegenStatement (MultiplyStatement _ x y z) = do
  x' <- codegenExpression x
  y' <- codegenExpression y
  z' <- getLocal z
  tell $ "NB_MULTIPLY(" <> intercalate ", " [z', x', y'] <> ");\n"

codegenExpression :: Expression PostCheck -> Codegen Builder
codegenExpression (VariableExpression _ Nothing name) = getLocal name
codegenExpression (VariableExpression _ (Just ns) name) =
  -- TODO(rightfold): Wrap function or procedure in nb::value.
  getGlobal ns name
codegenExpression (ApplyExpression _ applyee arguments) = do
  applyee' <- codegenExpression applyee
  arguments' <- traverse codegenExpression arguments
  result <- newTemporary
  tell $ "NB_APPLY(" <> intercalate ", " (result : applyee' : arguments') <> ");\n"
  pure result



newTemporary :: Codegen Builder
newTemporary = do
  index <- csNextTemporary <<+= 1
  let name = "T" <> string (show index)
  tell $ "nb::value " <> name <> ";\n"
  pure name

newQueryResult :: Codegen Builder
newQueryResult = do
  index <- csNextQueryResult <<+= 1
  let name = "Q" <> string (show index)
  tell $ "nb::query::result " <> name <> ";\n"
  pure name

newLocal :: VariableName -> Builder
newLocal (VariableName name) =
  "L" <> string (show (lengthUTF8 name)) <> text name

getLocal :: VariableName -> Codegen Builder
getLocal = pure . newLocal

newGlobal :: NamespaceName -> VariableName -> Builder
newGlobal (NamespaceName nss) (VariableName name) =
  fold [ "N" <> string (show (lengthUTF8 ns)) <> text ns | ns <- nss ] <>
  "G" <> string (show (lengthUTF8 name)) <> text name

getGlobal :: NamespaceName -> VariableName -> Codegen Builder
getGlobal = (pure .) . newGlobal

fullName :: NamespaceName -> VariableName -> Text
fullName = \ns name -> prettyNS ns <> "." <> prettyName name
  where prettyNS (NamespaceName []) = "NULL-NAMESPACE"
        prettyNS (NamespaceName ns) = intercalate "." ns
        prettyName (VariableName n) = n

stringLiteral :: Text -> Builder
stringLiteral = string . show

string :: String -> Builder
string = Builder.stringUtf8

text :: Text -> Builder
text = Builder.byteString . encodeUtf8

lengthUTF8 :: Text -> Word
lengthUTF8 = fromIntegral . ByteString.length . encodeUtf8

intercalate :: (Foldable f, Monoid a) => a -> f a -> a
intercalate sep = snd . foldl' go (True, mempty)
  where go (True, _)    x = (False, x)
        go (False, acc) x = (False, fold [acc, sep, x])
