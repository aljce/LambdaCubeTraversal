{-# LANGUAGE DeriveFunctor, TypeFamilies #-}
module SimplyTyped.Syntax where

import Data.Functor.Foldable
import Prelude hiding (Foldable)

import Text.PrettyPrint.ANSI.Leijen

type Name = String

data TypeF f = TF | TArrF f f deriving Functor

data Type = T | TArr Type Type deriving Eq

type instance Base Type = TypeF

instance Foldable Type where
  project T = TF
  project (TArr type1 type2) = TArrF type1 type2

typeToDoc :: Type -> Doc
typeToDoc = para alg
  where alg TF = char 't'
        alg (TArrF (subtype,type1) (_,type2)) = left <> text " -> " <> type2
          where left = case subtype of
                  TArr{} -> parens type1
                  _      -> type1

instance Show Type where
  show = show . typeToDoc

data TermF f = VarF Name |
               AbstracF Name Type f |
               ApplyF f f deriving Functor

data Term = Var Name |
            Abstrac Name Type Term |
            Apply Term Term deriving Eq

type instance Base Term = TermF

instance Foldable Term where
  project (Var name) = VarF name
  project (Abstrac name typ term) = AbstracF name typ term
  project (Apply term1 term2) = ApplyF term1 term2

termToDoc :: Term -> Doc
termToDoc = para alg
  where alg (VarF name) = text name
        alg (AbstracF name typ (_,term)) = hcat [backslash, text name, colon, typeToDoc typ, dot, term]
        alg (ApplyF (subterm1,term1) (subterm2,term2)) = t1Parens <+> t2Parens
          where t1Parens = case subterm1 of
                  Abstrac{} -> parens term1
                  _         -> term1
                t2Parens = case subterm2 of
                  Apply _ _ -> parens term2
                  _         -> term2

instance Show Term where
  show = show . termToDoc
