{-# LANGUAGE DeriveFunctor, TypeFamilies #-}
module UnTyped.Syntax where

import Data.Functor.Foldable
import Prelude hiding (Foldable)

import Data.Text hiding (empty)
import Text.PrettyPrint.ANSI.Leijen

data Lit = CBool Bool | CInt Int deriving Eq

instance Show Lit where
  show (CBool b) = show b
  show (CInt  i) = show i

type Name = Text

data TermF f = ConstantF Lit |
               VarF Name |
               AbstracF Name f |
               ApplyF f f deriving Functor

data Term = Constant Lit |
            Var Name |
            Abstrac Name Term |
            Apply Term Term deriving Eq

type instance Base Term = TermF

instance Foldable Term where
  project (Constant lit)      = ConstantF lit
  project (Var name)          = VarF name
  project (Abstrac name term) = AbstracF name term
  project (Apply term1 term2) = ApplyF term1 term2

instance Unfoldable Term where
  embed (ConstantF lit) = Constant lit
  embed (VarF name) = Var name
  embed (AbstracF name term) = Abstrac name term
  embed (ApplyF term1 term2) = Apply term1 term2

termToDoc :: Term -> Doc
termToDoc = para alg
  where textToDoc = text . unpack
        alg (ConstantF (CBool bol))  = pretty bol
        alg (ConstantF (CInt  lit))  = pretty lit
        alg (VarF name)              = textToDoc name --TODO: f : Text -> Doc???
        alg (AbstracF name (_,term)) = backslash <> textToDoc name <> dot <> term
        alg (ApplyF (subterm1,term1) (subterm2,term2)) = t1Parens <+> t2Parens 
          where t1Parens = case subterm1 of
                  Abstrac _ _ -> parens term1
                  _           -> term1
                t2Parens = case subterm2 of
                  Apply _ _ -> parens term2
                  _         -> term2
 
        -- nameAlg (AbstracF name subterm) = space <> textToDoc name <> subterm
        -- nameAlg _ = empty
-- termToDoc :: Term -> Doc
-- termToDoc = para alg
--   where alg (ConstantF (CBool bol))    = (0,pretty bol)
--         alg (ConstantF (CInt  lit))    = (0,pretty lit)
--         alg (VarF name)                = (0,textToDoc name) --TODO: f : Text -> Doc???
--         alg (AbstracF name (t,(n,term)))     = (0,backslash <> textToDoc name <> dot <> term)
--         alg (ApplyF (t,(n1,term1)) (s,(n2,term2))) = (0,parens term1 <+> parens term2)

instance Show Term where
  show = show . termToDoc
