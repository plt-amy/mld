module Typing.Unify (unify, mergeDelta) where

import qualified Data.Map.Merge.Strict as Map
import qualified Data.Set as Set

import Typing.Errors
import Typing.Monad

import Syntax.Subst
import Syntax

doUnify :: Type -> Type -> TypingM ()
doUnify (TyVar v) t =
  if (v `Set.member` ftv t) && (t /= TyVar v)
     then throwError (OccursError v t)
     else extendSub $ singleton v t
doUnify t (TyVar v) =
  if (v `Set.member` ftv t) && (t /= TyVar v)
     then throwError (OccursError v t)
     else extendSub $ singleton v t
doUnify (a :-> d) (a' :-> d') = do
  doUnify a a'
  join $
    unify <$> applySub d <*> applySub d'
unify (TyCon v) (TyCon v') | v == v' = pure ()
unify a b = throwError (NotEqualError a b)

unify :: Type -> Type -> TypingM ()
unify l r = join $ doUnify <$> applySub l <*> applySub r

mergeDelta :: Delta -> Delta -> TypingM Delta
mergeDelta (Delta da) (Delta db) = Delta <$> Map.mergeA keep keep try da db where
  keep = Map.preserveMissing
  try = Map.zipWithAMatched $ \v a b -> do
    unify a b
      `catchError` \e -> throwError (addMergeErrorCtx v e)
    applySub b
