module SimplyTyped.Check where

import Prelude hiding (lookup)

import Control.Monad.Reader
import Control.Monad.Except

import Data.Map.Strict

import SimplyTyped.Syntax

type Context = Map Name Type

data TypeMismatch = TypeMismatch {
  firstType  :: Type,
  secondType :: Type
} | NotInScope {
  varName :: Name
} | NotAFunction {
  nonFunType :: Type,
  nonFunTerm :: Term
} deriving Show

type Checked = ExceptT TypeMismatch (Reader Context)

typeCheck :: Term -> Checked Type
typeCheck (Var name) = do
  cntx <- ask
  case lookup name cntx of
    Just typ -> return typ
    Nothing  -> throwError (NotInScope name)
typeCheck (Abstrac name typ term) = local (insert name typ) (TArr typ <$> typeCheck term)
typeCheck (Apply term1 term2) = do
  type1 <- typeCheck term1
  type2 <- typeCheck term2
  case type1 of
    TArr fstArr sndArr -> case fstArr == type2 of
      True  -> return sndArr
      False -> throwError (TypeMismatch fstArr type2)
    T -> throwError (NotAFunction type1 term1)

runCheck :: Term -> Either TypeMismatch Type
runCheck term = runReader (runExceptT (typeCheck term)) empty
