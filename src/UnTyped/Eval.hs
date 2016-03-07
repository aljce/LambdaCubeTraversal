module UnTyped.Eval where

import Prelude hiding (lookup)

import Data.Functor.Foldable
import Control.Applicative

import qualified Data.Set as S

-- import Data.Map.Strict

import UnTyped.Syntax
import UnTyped.Parser

-- type Context = Map Name Term

freeVars :: Term -> S.Set Name
freeVars = cata alg
  where alg (ConstantF _) = S.empty
        alg (VarF name)   = S.singleton name
        alg (ApplyF term1 term2) = term1 `S.union` term2
        alg (AbstracF name term) = S.delete name term

substitute :: Name -> Term -> Term -> Term
substitute meta newT = para sub
  where sub (VarF name) = case meta == name of
          True  -> newT
          False -> Var name
        sub (ApplyF term1 term2) = Apply (snd term1) (snd term2)
        sub (AbstracF name (oldterm,newterm)) = case meta /= name && S.notMember name (freeVars newT) of
          True  -> Abstrac name newterm
          False -> Abstrac name oldterm
        sub (ConstantF lit) = Constant lit

stepEval :: Term -> Term
stepEval = para alg
  where alg (ApplyF (Abstrac name term1,_) (term2,_)) = substitute name term2 term1
        alg (ApplyF (_,term1) (_,term2)) = Apply term1 term2
        alg (AbstracF name (_,term)) = Abstrac name term
        alg (ConstantF lit) = Constant lit
        alg (VarF name)     = Var name

(Right s) = parse "\\x.\\y.\\z.x z (y z)"

(Right k) = parse "\\x.\\y.x"

(Right i) = parse "\\x.x"

skk = Apply (Apply s k) k

