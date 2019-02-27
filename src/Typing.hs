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
infer (Use a v) = do
  typing <- getTyping v
  case typing of
    Just dt -> refresh a dt
    Nothing -> do
      ty <- TyVar <$> newVar
      pure $ Typing (Just a) (typedAs v a ty) ty

infer (Lam a var body) = do
  Typing _ delta sigma <- infer body
  case delta ! var of
    Just (Spanned _ tau) ->
      pure $ Typing (Just a) (delta \- var) (tau :-> sigma)
    Nothing -> do
      alpha <- TyVar <$> newVar
      pure $ Typing (Just a) delta (alpha :-> sigma)

infer (App a fun arg) = do
  Typing _ fun_d fun_t <- infer fun
  Typing arg_s arg_d arg_t <- infer arg

  alpha <- TyVar <$> newVar
  unify fun_t (arg_t :-> alpha)
    `catchError` noted
       [ Snippet arg_s arg_t
       , "But the function was expecting an argument of type"
       , Line $ "  " ++ show (domain fun_t)
       ]

  delta <- fun_d `mergeDelta` arg_d
  applySub (Typing (Just a) delta alpha)

infer (Let a (v, e) body) = do
  dt@(Typing _ let_d _) <- reduce v <$> infer e
  Typing _ body_d tau <- local (insertGamma v dt) $
    infer body
  overall_d <- mergeDelta let_d body_d
  pure (Typing (Just a) overall_d tau)

infer (Num a _) = pure $ Typing (Just a) mempty (TyCon "Int")

reduce :: Var -> Typing -> Typing
reduce v (Typing a (Delta delta) tau) =
  let tau_fv = ftv tau
      delta' = Map.filter keep (Map.delete v delta)
      keep sigma = not $ Set.null (ftv sigma `Set.intersection` tau_fv)
   in Typing a (Delta delta') tau

domain :: Type -> Type
domain (x :-> _) = x
domain t = t
