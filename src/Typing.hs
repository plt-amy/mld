{-# LANGUAGE OverloadedStrings #-}
module Typing (infer) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Typing.Monad
import Typing.Unify

import Syntax.Subst
import Syntax

infer :: Exp -> TypingM Typing
infer (Use v) = do
  typing <- getTyping v
  case typing of
    Just dt -> refresh dt
    Nothing -> do
      ty <- TyVar <$> newVar
      pure $ v `typedAs` ty ⊢ ty

infer (Lam var body) = do
  Typing delta sigma <- infer body
  case delta ! var of
    Just tau -> pure $ (delta \- var) ⊢ (tau :-> sigma)
    Nothing -> do
      alpha <- TyVar <$> newVar
      pure $ delta ⊢ (alpha :-> sigma)

infer (App fun arg) = do
  Typing fun_d fun_t <- infer fun
  Typing arg_d arg_t <- infer arg

  alpha <- TyVar <$> newVar
  unify fun_t (arg_t :-> alpha)

  delta <- fun_d `mergeDelta` arg_d
  applySub (delta ⊢ alpha) 

infer (Let (v, e) body) = do
  dt <- reduce v <$> infer e
  local (insertGamma v dt) $
    infer body

infer (Num _) = pure $ mempty ⊢ TyCon "Num"

reduce :: Var -> Typing -> Typing
reduce v (Typing (Delta delta) tau) =
  let tau_fv = ftv tau
      delta' = Map.filter keep (Map.delete v delta)
      keep sigma = not $ Set.null (ftv sigma `Set.intersection` tau_fv)
   in Typing (Delta delta') tau
