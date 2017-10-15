{-# LANGUAGE TemplateHaskell #-}

module Niobiumc.Codegen.Object
  ( Object (..)
  , Closure (..)
  , Chunk (..)
  , Instruction (..)
  , Source (..)
  , Destination (..)
  , Value (..)
  , Global (..)
  , Local (..)
  , objectName

  , write
  , writeMagic
  , writeVersion
  , writeObjectKind
  , writeClosure
  , writeChunk
  , writeInstruction
  , writeSource
  , writeDestination
  , writeConstant
  , writeLocal
  , writeGlobal
  , writeType
  , writeArray
  , writeName
  , writeString
  , writeU8
  , writeU16
  , writeI32
  , writeU32

  , ObjectgenT
  , Objectgen
  , runObjectgenT
  , runObjectgen

  , CodegenT
  , Codegen
  , CodegenEnvironment (..)
  , CodegenState (..)
  , runCodegenT
  , runCodegen

  , objectgenDeclaration
  , codegenStatement
  , codegenExpression
  ) where

import Control.Lens ((<<+=), (%~), makeLenses, view)
import Control.Monad.RWS (RWST, runRWST)
import Data.ByteString.Builder (Builder)
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int32)
import Data.List (genericLength, intersperse)
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Monoid (Sum (..))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8, Word16, Word32)
import Niobiumc.Syntax (Declaration (..), Expression (..), NamespaceName (..), PostCheck, Statement (..), Type (..), VariableName (..), expressionAnnotation)

import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Writer as Writer
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.Map as Map



data Object
  = ProcedureObject Closure
  | FunctionObject Closure

newtype Closure = Closure Chunk

data Chunk = Chunk
  { chunkName :: (NamespaceName, VariableName)
  , chunkLocalCount :: Word32
  , chunkInstructions :: [Instruction]
  }

data Instruction
  = AddIntInstruction Source Source Destination
  | MultiplyIntInstruction Source Source Destination

  | CallProcedureInstruction Source [Source] [Destination]
  | ReturnFromProcedureInstruction

  | ReturnFromFunctionInstruction Source

  | forall a. MakeReportHandlerInstruction Source [Type a] [Type a] Destination

data Source
  = ConstantSource Value
  | GlobalSource Global
  | LocalSource Local
  deriving (Eq, Ord)

data Destination
  = DiscardDestination
  | LocalDestination Local
  deriving (Eq, Ord)

data Value
  = IntValue Int32
  deriving (Eq, Ord)

newtype Global = Global (NamespaceName, VariableName)
  deriving (Eq, Ord)

newtype Local = Local Word32
  deriving (Eq, Ord)

objectName :: Object -> (NamespaceName, VariableName)
objectName (FunctionObject (Closure chunk)) = chunkName chunk
objectName (ProcedureObject (Closure chunk)) = chunkName chunk



write :: Object -> Builder
write object = mempty
  <> writeMagic
  <> writeVersion
  <> writeObjectKind object
  <> case object of
       FunctionObject closure -> writeClosure closure
       ProcedureObject closure -> writeClosure closure

writeMagic :: Builder
writeMagic = Builder.byteString . ByteString.pack $
  [0x53, 0x49, 0x4E, 0x45, 0x57, 0x41, 0x56, 0x45]

writeVersion :: Builder
writeVersion = Builder.byteString . ByteString.pack $
  [0, 0]

writeObjectKind :: Object -> Builder
writeObjectKind (FunctionObject _) = writeU8 0x01
writeObjectKind (ProcedureObject _) = writeU8 0x00

writeClosure :: Closure -> Builder
writeClosure (Closure chunk) = writeChunk chunk

writeChunk :: Chunk -> Builder
writeChunk (Chunk name localCount instructions) = mempty
  <> writeName name
  <> writeU32 localCount
  <> foldMap writeInstruction instructions

writeInstruction :: Instruction -> Builder
writeInstruction (AddIntInstruction sourceA sourceB destination) = mempty
  <> writeU16 0x00
  <> writeSource sourceA
  <> writeSource sourceB
  <> writeDestination destination
writeInstruction (MultiplyIntInstruction sourceA sourceB destination) = mempty
  <> writeU16 0x01
  <> writeSource sourceA
  <> writeSource sourceB
  <> writeDestination destination
writeInstruction (CallProcedureInstruction callee using giving) = mempty
  <> writeU16 0x10
  <> writeSource callee
  <> writeArray writeSource using
  <> writeArray writeDestination giving
writeInstruction ReturnFromProcedureInstruction = mempty
  <> writeU16 0x11
writeInstruction (ReturnFromFunctionInstruction source) = mempty
  <> writeU16 0x20
  <> writeSource source
writeInstruction (MakeReportHandlerInstruction procedure using giving destination) = mempty
  <> writeU16 0x30
  <> writeSource procedure
  <> writeArray writeType using
  <> writeArray writeType giving
  <> writeDestination destination

writeSource :: Source -> Builder
writeSource (ConstantSource value) = writeU8 0x00 <> writeConstant value
writeSource (GlobalSource global)  = writeU8 0x01 <> writeGlobal global
writeSource (LocalSource local)    = writeU8 0x02 <> writeLocal local

writeDestination :: Destination -> Builder
writeDestination DiscardDestination       = writeU8 0x00
writeDestination (LocalDestination local) = writeU8 0x01 <> writeLocal local

writeConstant :: Value -> Builder
writeConstant (IntValue value) = writeU8 0x00 <> writeI32 value

writeLocal :: Local -> Builder
writeLocal (Local local) = writeU32 local

writeGlobal :: Global -> Builder
writeGlobal (Global global) = writeName global

writeType :: Type a -> Builder
writeType (IntType _) = writeU8 0x00
writeType _ = error "NYI: writeType"

writeArray :: (a -> Builder) -> [a] -> Builder
writeArray f xs = mempty
  <> writeU32 (genericLength xs)
  <> foldMap f xs

writeName :: (NamespaceName, VariableName) -> Builder
writeName (NamespaceName ns, VariableName name) =
  writeString . mconcat . intersperse "." $ ns <> [name]

writeString :: Text -> Builder
writeString string = mempty
  <> writeU32 (fromIntegral . ByteString.length $ encoding)
  <> Builder.byteString encoding
  where encoding = encodeUtf8 string

writeU8 :: Word8 -> Builder
writeU8 = Builder.word8

writeU16 :: Word16 -> Builder
writeU16 = Builder.word16BE

writeI32 :: Int32 -> Builder
writeI32 = Builder.int32BE

writeU32 :: Word32 -> Builder
writeU32 = Builder.word32BE



type ObjectgenT = RWST () [Object] ()

type Objectgen = ObjectgenT Identity

runObjectgenT :: Monad m => ObjectgenT m a -> m (a, [Object])
runObjectgenT action = do { (a, (), w) <- runRWST action () (); pure (a, w) }

runObjectgen :: Objectgen a -> (a, [Object])
runObjectgen = runIdentity . runObjectgenT



type CodegenT = RWST CodegenEnvironment (Sum Word32, [Instruction]) CodegenState

type Codegen = CodegenT Identity

data CodegenEnvironment = CodegenEnvironment
  { _ceLocals :: Map VariableName Local
  }

data CodegenState = CodegenState
  { _csNextLocal :: Word32
  }

$(makeLenses ''CodegenEnvironment)
$(makeLenses ''CodegenState)

runCodegenT :: Monad m => CodegenT m a -> m (a, Word32, [Instruction])
runCodegenT action = do { (a, _, (Sum l, i)) <- runRWST action environment state
                        ; pure (a, l, i) }
  where environment = CodegenEnvironment { _ceLocals = Map.empty }
        state = CodegenState { _csNextLocal = 0 }

runCodegen :: Codegen a -> (a, Word32, [Instruction])
runCodegen = runIdentity . runCodegenT



objectgenDeclaration :: Monad m => Declaration PostCheck -> ObjectgenT m ()
objectgenDeclaration (NamespaceDeclaration _ _) = pure ()
objectgenDeclaration (UsingDeclaration _ _) = pure ()
objectgenDeclaration (FunctionDeclaration (_, ns) name parameters _ body) =
  objectgenFunction ns name (fst <$> parameters) body
objectgenDeclaration (ProcedureDeclaration (_, ns) name using giving body) =
  objectgenProcedure ns name (fst <$> using) (fst <$> giving) body

objectgenFunction
  :: Monad m
  => NamespaceName
  -> VariableName
  -> [VariableName]
  -> Expression PostCheck
  -> ObjectgenT m ()
objectgenFunction ns name parameters body = do
  ((), localCount, instructions) <- runCodegenT $ do
    parameters' <- traverse ((<$> codegenFreshLocal') . (,)) parameters
    Reader.local (ceLocals %~ Map.union (Map.fromList parameters')) $ do
      result <- codegenExpression body
      codegenInstruction $ ReturnFromFunctionInstruction result
  let chunk = Chunk { chunkName = (ns, name)
                    , chunkLocalCount = localCount
                    , chunkInstructions = instructions }
  Writer.tell [FunctionObject . Closure $ chunk]

objectgenProcedure
  :: Monad m
  => NamespaceName
  -> VariableName
  -> [VariableName]
  -> [VariableName]
  -> [Statement PostCheck]
  -> ObjectgenT m ()
objectgenProcedure ns name using giving body = do
  ((), localCount, instructions) <- runCodegenT $ do
    using' <- traverse ((<$> codegenFreshLocal') . (,)) using
    giving' <- traverse ((<$> codegenFreshLocal') . (,)) giving
    Reader.local (ceLocals %~ Map.union (Map.fromList (using' <> giving'))) $ do
      traverse_ codegenStatement body
      codegenInstruction ReturnFromProcedureInstruction
  let chunk = Chunk { chunkName = (ns, name)
                    , chunkLocalCount = localCount
                    , chunkInstructions = instructions }
  Writer.tell [ProcedureObject . Closure $ chunk]

codegenStatement :: Monad m => Statement PostCheck -> CodegenT m ()
codegenStatement (AddStatement _ termA termB result) = do
  termA' <- codegenExpression termA
  termB' <- codegenExpression termB
  destination <- LocalDestination <$> codegenLookupLocal result
  codegenInstruction $ AddIntInstruction termA' termB' destination
codegenStatement (CallStatement _ callee using giving) = do
  callee' <- codegenExpression callee
  using' <- traverse codegenExpression using
  giving' <- traverse (fmap LocalDestination . codegenLookupLocal) giving
  codegenInstruction $ CallProcedureInstruction callee' using' giving'
codegenStatement (ExecuteQueryStatement _ _ _ _ _) = error "NYI: ExecuteQueryStatement"
codegenStatement (ForEachStatement _ _ _ _) = error "NYI: ForEachStatement"
codegenStatement (MultiplyStatement _ termA termB result) = do
  termA' <- codegenExpression termA
  termB' <- codegenExpression termB
  destination <- LocalDestination <$> codegenLookupLocal result
  codegenInstruction $ MultiplyIntInstruction termA' termB' destination

codegenExpression :: Monad m => Expression PostCheck -> CodegenT m Source
codegenExpression (VariableExpression _ (Just ns) name) = pure $ GlobalSource (Global (ns, name))
codegenExpression (VariableExpression _ Nothing name) = LocalSource <$> codegenLookupLocal name
codegenExpression (ApplyExpression _ _ _) = error "NYI: ApplyExpression"
codegenExpression (ReportHandlerExpression _ subroutine) = do
  let (using, giving) = case snd . expressionAnnotation $ subroutine of
        FunctionType _ parameters returnType -> (parameters, [returnType])
        ProcedureType _ using' giving' -> (using', giving')
        _ -> error "top kek"
  subroutine' <- codegenExpression subroutine
  (resultS, resultD) <- codegenFreshLocal
  codegenInstruction $ MakeReportHandlerInstruction subroutine' using giving resultD
  pure resultS

codegenLookupLocal :: Monad m => VariableName -> CodegenT m Local
codegenLookupLocal name =
  fmap fromJust . Reader.asks $ Map.lookup name . view ceLocals

codegenFreshLocal :: Monad m => CodegenT m (Source, Destination)
codegenFreshLocal = do
  local <- codegenFreshLocal'
  pure (LocalSource local, LocalDestination local)

codegenFreshLocal' :: Monad m => CodegenT m Local
codegenFreshLocal' = do
  Writer.tell (1, [])
  Local <$> (csNextLocal <<+= 1)

codegenInstruction :: Monad m => Instruction -> CodegenT m ()
codegenInstruction instruction = Writer.tell (0, [instruction])
