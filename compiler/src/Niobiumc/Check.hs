{-# LANGUAGE TemplateHaskell #-}

module Niobiumc.Check
  ( Check
  , CheckError (..)
  , CheckEnvironment (..)
  , CheckState (..)
  , runCheck

  , checkDeclaration
  , checkExpression
  , checkStatement
  , checkType
  ) where

import Control.Lens ((%~), (?~), (.=), (?=), at, makeLenses, use, view)
import Control.Monad (when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.RWS (RWST, evalRWST)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Semigroup ((<>))
import Niobiumc.Syntax (Declaration (..), Expression (..), NamespaceName (..), Position, PostCheck, PostParse, Statement (..), Type (..), VariableName (..), expressionAnnotation, typeOf)

import qualified Control.Monad.Reader as Reader
import qualified Data.Map as Map



type Check = RWST CheckEnvironment () CheckState (Either CheckError)

data CheckError
  = ApplyeeNotFunction Position (Type PostCheck)
  | ArithmeticTypeMismatch Position
  | CalleeNotProcedure Position (Type PostCheck)
  | InvalidArgumentCount Position (Type PostCheck)
  | NotIterable Position
  | ReportImplementationNotSubroutine Position
  | TypeMismatch Position
  | UnknownVariableError Position VariableName
  deriving (Show)

data CheckEnvironment = CheckEnvironment
  { _ceVariables :: Map VariableName (Type PostCheck)
  }
  deriving (Show)

data CheckState = CheckState
  { _csCurrentNamespace :: NamespaceName
  , _csGlobalVariables :: Map (NamespaceName, VariableName) (Type PostCheck)
  }
  deriving (Show)

$(makeLenses ''CheckEnvironment)
$(makeLenses ''CheckState)

runCheck :: Check a -> Either CheckError a
runCheck action = fst <$> evalRWST action environment state
  where environment = CheckEnvironment { _ceVariables = Map.empty }
        state = CheckState { _csCurrentNamespace = NamespaceName []
                           , _csGlobalVariables = Map.empty
                           }



checkDeclaration :: Declaration PostParse -> Check (Declaration PostCheck)
checkDeclaration (NamespaceDeclaration a ns) = do
  currentNS <- use csCurrentNamespace
  csCurrentNamespace .= ns
  pure $ NamespaceDeclaration (a, currentNS) ns
checkDeclaration (UsingDeclaration a ns) = do
  currentNS <- use csCurrentNamespace
  -- TODO(rightfold): Bring leaf into scope as namespace alias.
  -- TODO(rightfold): Bring declarations with same name as leaf into scope.
  pure $ UsingDeclaration (a, currentNS) ns
checkDeclaration (FunctionDeclaration a name parameters returnType body) = do
  currentNS <- use csCurrentNamespace

  parameters' <- traverse (traverse checkType) parameters
  returnType' <- checkType returnType

  csGlobalVariables . at (currentNS, name) ?=
    FunctionType a (snd <$> parameters') returnType'

  let parameterVariables = Map.fromList parameters'
      bodyEnvironment = ceVariables %~ (parameterVariables <>)
  body' <- Reader.local bodyEnvironment $ checkExpression body

  pure $ FunctionDeclaration (a, currentNS) name parameters' returnType' body'
checkDeclaration (ProcedureDeclaration a name using giving body) = do
  currentNS <- use csCurrentNamespace

  using' <- traverse (traverse checkType) using
  giving' <- traverse (traverse checkType) giving

  csGlobalVariables . at (currentNS, name) ?=
    ProcedureType a (snd <$> using') (snd <$> giving')

  let usingVariables = Map.fromList using'
  let givingVariables = Map.fromList giving'
  let bodyEnvironment = ceVariables %~ ((usingVariables <> givingVariables) <>)
  body' <- Reader.local bodyEnvironment $ traverse checkStatement body

  pure $ ProcedureDeclaration (a, currentNS) name using' giving' body'

checkStatement :: Statement PostParse -> Check (Statement PostCheck)
checkStatement (AddStatement a x y z) = do
  x' <- checkExpression x
  y' <- checkExpression y
  z' <- checkExpression (VariableExpression a Nothing z)
  case (typeOf x', typeOf y', typeOf z') of
    (IntType _, IntType _, IntType _) -> pure $ AddStatement a x' y' z
    _ -> throwError $ ArithmeticTypeMismatch a
checkStatement (CallStatement a callee using giving) = do
  callee' <- checkExpression callee
  using' <- traverse checkExpression using
  giving' <- traverse (checkGiving a) giving
  case typeOf callee' of
    ty@(ProcedureType _ calleeUsing calleeGiving)
      | length calleeUsing /= length using' ->
          throwError $ InvalidArgumentCount a ty
      | length calleeGiving /= length giving' ->
          throwError $ InvalidArgumentCount a ty
      | otherwise -> do
          using'' <- sequence [ checkCoerce a p e | p <- calleeUsing
                                                  | e <- using' ]
          -- TODO(rightfold): Coerce giving parameters.
          pure $ CallStatement a callee' using'' giving
    ty -> throwError $ CalleeNotProcedure a ty
checkStatement (ExecuteQueryStatement a query using giving resultAction) = do
  -- TODO(rightfold): Type-check the SQL query using @PQdescribePrepared@.
  using' <- traverse checkExpression using
  traverse_ (checkGiving a) giving
  pure $ ExecuteQueryStatement a query using' giving resultAction
checkStatement (ForEachStatement a name iterable body) = do
  (iterable', elementType) <- checkIterable iterable
  let bodyEnvironment = ceVariables . at name ?~ elementType
  body' <- Reader.local bodyEnvironment $
    traverse checkStatement body
  pure $ ForEachStatement a name iterable' body'
checkStatement (MultiplyStatement a x y z) = do
  x' <- checkExpression x
  y' <- checkExpression y
  z' <- checkExpression (VariableExpression a Nothing z)
  case (typeOf x', typeOf y', typeOf z') of
    (IntType _, IntType _, IntType _) -> pure $ MultiplyStatement a x' y' z
    _ -> throwError $ ArithmeticTypeMismatch a

checkExpression :: Expression PostParse -> Check (Expression PostCheck)
checkExpression (ApplyExpression a applyee arguments) = do
  applyee' <- checkExpression applyee
  arguments' <- traverse checkExpression arguments
  case typeOf applyee' of
    ty@(FunctionType _ parameters returnType)
      | length parameters /= length arguments' ->
          throwError $ InvalidArgumentCount a ty
      | otherwise -> do
          arguments'' <- sequence [ checkCoerce a p e | p <- parameters
                                                      | e <- arguments' ]
          pure $ ApplyExpression (a, returnType) applyee' arguments''
    ty -> throwError $ ApplyeeNotFunction a ty
checkExpression (ReportInterfaceExpression a subroutine) = do
  subroutine' <- checkExpression subroutine
  case typeOf subroutine' of
    FunctionType _ _ _ -> pure ()
    ProcedureType _ _ _ -> pure ()
    _ -> throwError $ ReportImplementationNotSubroutine a
  pure $ ReportInterfaceExpression (a, ReportInterfaceType a) subroutine'
checkExpression (VariableExpression _ (Just _) _) =
  error "Not yet implemented: checkExpression on qualified variables."
checkExpression (VariableExpression a Nothing name) = do
  currentNS <- use csCurrentNamespace
  local <- Reader.asks (Map.lookup name . view ceVariables)
  global <- use $ csGlobalVariables . at (currentNS, name)
  case (local, global) of
    (Nothing, Nothing) -> throwError (UnknownVariableError a name)
    (Just ty, _)       -> pure $ VariableExpression (a, ty) Nothing name
    (Nothing, Just ty) -> pure $ VariableExpression (a, ty) (Just currentNS) name

checkType :: Type PostParse -> Check (Type PostCheck)
checkType (FunctionType a parameters returnType) = do
  parameters' <- traverse checkType parameters
  returnType' <- checkType returnType
  pure $ FunctionType a parameters' returnType'
checkType (IntType a) =
  pure $ IntType a
checkType (ProcedureType a using giving) = do
  using' <- traverse checkType using
  giving' <- traverse checkType giving
  pure $ ProcedureType a using' giving'
checkType (ReportInterfaceType a) =
  pure $ ReportInterfaceType a



checkGiving :: Position -> VariableName -> Check (Type PostCheck)
checkGiving a = fmap typeOf . checkExpression . asExpression
  where asExpression = VariableExpression a Nothing

checkCoerce :: Position -> Type PostCheck -> Expression PostCheck -> Check (Expression PostCheck)
checkCoerce a = \ty e -> e <$ checkAssignable ty (typeOf e)
  where
    checkAssignable :: Type PostCheck -> Type PostCheck -> Check ()
    checkAssignable (FunctionType _ ps1 r1) (FunctionType _ ps2 r2) = do
      when (length ps1 /= length ps2) $
        throwError $ TypeMismatch a
      sequence_ [checkAssignable p1 p2 | p1 <- ps1 | p2 <- ps2]
      checkAssignable r1 r2
    checkAssignable (IntType _) (IntType _) = pure ()
    checkAssignable (ProcedureType _ us1 gs1) (ProcedureType _ us2 gs2) = do
      when (length us1 /= length us2) $
        throwError $ TypeMismatch a
      when (length gs1 /= length gs2) $
        throwError $ TypeMismatch a
      sequence_ [checkAssignable u1 u2 | u1 <- us1 | u2 <- us2]
      sequence_ [checkAssignable g1 g2 | g1 <- gs1 | g2 <- gs2]
    checkAssignable (ReportInterfaceType _) (ReportInterfaceType _) = pure ()
    checkAssignable _ _ = throwError $ TypeMismatch a

checkIterable :: Expression PostParse -> Check (Expression PostCheck, Type PostCheck)
checkIterable iterable = do
  iterable' <- checkExpression iterable
  elementType <- case typeOf iterable' of
    _ -> throwError $ NotIterable (expressionAnnotation iterable)
  pure (iterable', elementType)
