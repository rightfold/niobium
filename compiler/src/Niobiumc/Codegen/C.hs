{-# LANGUAGE TemplateHaskell #-}

module Niobiumc.Codegen.C
  ( Codegen
  , CodegenEnvironment (..)
  , CodegenState (..)
  , runCodegen

  , codegenHeader
  , codegenFooter

  , codegenDeclaration
  , codegenStatement
  , codegenExpression
  ) where

import Control.Lens ((<>~), (.=), (<<+=), at, makeLenses, use, view)
import Control.Monad.RWS (RWS, asks, local, runRWS, tell)
import Data.ByteString.Builder (Builder)
import Data.Foldable (fold, foldl', traverse_)
import Data.Maybe (isJust)
import Data.Semigroup ((<>))
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Niobiumc.Syntax (Declaration (..), ExecuteQueryResultAction (..), Expression (..), NamespaceName (..), PostCheck, Statement (..), VariableName (..))

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.Set as Set



type Codegen = RWS CodegenEnvironment Builder CodegenState

data CodegenEnvironment = CodegenEnvironment
  { _ceReferences :: Set VariableName
  }

data CodegenState = CodegenState
  { _csNextTemporary :: Word
  , _csNextQueryResult :: Word
  , _csCurrentNamespace :: NamespaceName
  }

$(makeLenses ''CodegenEnvironment)
$(makeLenses ''CodegenState)

runCodegen :: Codegen a -> (a, Builder)
runCodegen action = let (a, _, c) = runRWS action environment state in (a, c)
  where environment = CodegenEnvironment { _ceReferences = Set.empty }
        state = CodegenState { _csNextTemporary = 0
                             , _csNextQueryResult = 0
                             , _csCurrentNamespace = NamespaceName []
                             }



codegenHeader :: Codegen ()
codegenHeader = do
  tell $ "#include <niobium/context.h>\n"
  tell $ "#include <niobium/evaluation.h>\n"
  tell $ "#include <niobium/value.h>\n"

codegenFooter :: Codegen ()
codegenFooter = pure ()



codegenDeclaration :: Declaration PostCheck -> Codegen ()
codegenDeclaration (NamespaceDeclaration _ ns) = csCurrentNamespace .= ns
codegenDeclaration (UsingDeclaration _ _) = pure ()
codegenDeclaration (FunctionDeclaration _ name parameters _ body) = do
  currentNS <- use csCurrentNamespace
  let parameters' = [ "nb_value " <> newLocal p | (p, _) <- parameters ]
  tell $ "nb_value " <> newGlobal currentNS name
  tell $ "(" <> intercalate ", " parameters' <> ") {\n"
  result <- codegenExpression body
  tell $ "return " <> result <> ";\n"
  tell $ "}\n"
codegenDeclaration (ProcedureDeclaration _ name using giving body) = do
  currentNS <- use csCurrentNamespace
  let using' = [ "nb_value " <> newLocal u | (u, _) <- using ]
      giving' = [ "nb_value *" <> newLocal g | (g, _) <- giving ]
      context = "nb_context *context"
  tell $ "void " <> newGlobal currentNS name
  tell $ "(" <> intercalate ", " (context : using' <> giving') <> ") {\n"
  tell $ "nb_enter_procedure(context, " <> stringLiteral (fullName currentNS name) <> ");\n"
  local (ceReferences <>~ Set.fromList [ g | (g, _) <- giving ]) $
    traverse_ codegenStatement body
  tell $ "nb_leave_procedure(context);\n"
  tell $ "}\n"

codegenStatement :: Statement PostCheck -> Codegen ()
codegenStatement (CallStatement _ callee using giving) = do
  callee' <- codegenExpression callee
  using' <- traverse codegenExpression using
  giving' <- sequence [ ("&" <>) <$> getLocal g | g <- giving ]
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
      tell $ "NB_NEXT_QUERY_ROW(" <> intercalate ", " (result : giving') <> ");\n"
codegenStatement (ForEachStatement _ name iterable body) = do
  name' <- getLocal name
  iterable' <- codegenExpression iterable
  tell $ "NB_FOR_EACH(" <> name' <> ", " <> iterable' <> ", {\n"
  traverse_ codegenStatement body
  tell $ "});\n"

codegenExpression :: Expression PostCheck -> Codegen Builder
codegenExpression (VariableExpression _ Nothing name) = getLocal name
codegenExpression (VariableExpression _ (Just ns) name) =
  -- TODO(rightfold): Wrap function or procedure in nb_value.
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
  tell $ "nb_value " <> name <> ";\n"
  pure name

newQueryResult :: Codegen Builder
newQueryResult = do
  index <- csNextQueryResult <<+= 1
  let name = "Q" <> string (show index)
  tell $ "nb_query_result " <> name <> ";\n"
  pure name

newLocal :: VariableName -> Builder
newLocal (VariableName name) =
  "L" <> string (show (lengthUTF8 name)) <> text name

getLocal :: VariableName -> Codegen Builder
getLocal name = do
  isReference <- isJust <$> asks (view $ ceReferences . at name)
  let prefix | isReference = "*"
             | otherwise = ""
  pure $ prefix <> newLocal name

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
