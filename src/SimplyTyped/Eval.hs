module SimplyTyped.Eval where

import Data.Functor.Foldable

import qualified Data.Set as S

import SimplyTyped.Syntax

freeVars :: Term -> S.Set Name
freeVars = cata alg
  where alg (VarF name)   = S.singleton name
        alg (ApplyF term1 term2) = term1 `S.union` term2
        alg (AbstracF name _ term) = S.delete name term

substitute :: Name -> Term -> Term -> Term
substitute meta newT = para sub
  where sub (VarF name) = case meta == name of
          True  -> newT
          False -> Var name
        sub (ApplyF term1 term2) = Apply (snd term1) (snd term2)
        sub (AbstracF name typ (oldterm,newterm)) = case meta /= name && S.notMember name (freeVars newT) of
          True  -> Abstrac name typ newterm
          False -> Abstrac name typ oldterm

eval :: Term -> Term
eval = para alg
  where alg (ApplyF (Abstrac name _ term1,_) (term2,_)) = substitute name term2 term1
        alg (ApplyF (_,term1) (_,term2)) = Apply term1 term2
        alg (AbstracF name typ (_,term)) = Abstrac name typ term
        alg (VarF name) = Var name

fullEval :: Term -> [Term]
fullEval = loop . (:[])
  where loop (oldterm:terms) = let newterm = eval oldterm in case oldterm == newterm of
          True  -> reverse (oldterm:terms)
          False -> loop (newterm:oldterm:terms)
