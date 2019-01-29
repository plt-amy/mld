module Typing.Unify where

import qualified Data.Map.Merge.Strict as Map
import qualified Data.Set as Set

import Typing.Errors
import Typing.Monad

import Syntax.Subst
import Syntax

unify :: Type -> Type -> TypingM ()
unify (TyVar v) t =
  if (v `Set.member` ftv t) && (t /= TyVar v)
     then throwError (occursError v t)
     else extendSub $ singleton v t
unify t (TyVar v) =
  if (v `Set.member` ftv t) && (t /= TyVar v)
     then throwError (occursError v t)
     else extendSub $ singleton v t
unify (a :-> d) (a' :-> d') = do
  unify a a'
  join $
    unify <$> applySub d <*> applySub d'
unify (TyCon v) (TyCon v') | v == v' = pure ()
unify a b = throwError (notEqualError a b)

mergeDelta :: Delta -> Delta -> TypingM Delta
mergeDelta (Delta da) (Delta db) = Delta <$> Map.mergeA keep keep try da db where
  keep = Map.preserveMissing
  try = Map.zipWithAMatched $ \v a b -> do
    unify a b
      `catchError` \e -> throwError (addMergeErrorCtx v e)
    applySub b
