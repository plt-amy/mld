{-# LANGUAGE OverloadedStrings #-}
module Typing (infer) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Typing.Errors
import Typing.Monad
import Typing.Unify

import Syntax.Subst
import Syntax

infer :: Exp -> TypingM Typing
infer x = infer' x `catchError` (throwError . addErrorSpan (expAnn x))

infer' :: Exp -> TypingM Typing
infer' (Use a v) = do
  typing <- getTyping v
  case typing of
    Just dt -> refresh dt
    Nothing -> do
      ty <- TyVar <$> newVar
      pure $ Typing (Just a) (v `typedAs` ty) ty

infer' (Lam a var body) = do
  Typing _ delta sigma <- infer body
  case delta ! var of
    Just tau -> pure $ Typing (Just a) (delta \- var) (tau :-> sigma)
    Nothing -> do
      alpha <- TyVar <$> newVar
      pure $ Typing (Just a) delta (alpha :-> sigma)

infer' (App a fun arg) = do
  Typing _ fun_d fun_t <- infer fun
  Typing _ arg_d arg_t <- infer arg

  alpha <- TyVar <$> newVar
  unify fun_t (arg_t :-> alpha)

  delta <- fun_d `mergeDelta` arg_d
  applySub (Typing (Just a) delta alpha)

infer' (Let a (v, e) body) = do
  dt <- reduce v <$> infer e
  withTypeAnn a <$> local (insertGamma v dt) (infer body)

infer' (Num a _) = pure $ Typing (Just a) mempty (TyCon "Int")

reduce :: Var -> Typing -> Typing
reduce v (Typing a (Delta delta) tau) =
  let tau_fv = ftv tau
      delta' = Map.filter keep (Map.delete v delta)
      keep sigma = not $ Set.null (ftv sigma `Set.intersection` tau_fv)
   in Typing a (Delta delta') tau
