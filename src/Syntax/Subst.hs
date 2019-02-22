{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Syntax.Subst (HasSubst(..), Subst, singleton, substFromList) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Syntax

class HasSubst a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set Var

newtype Subst = Subst { getSubst :: Map.Map Var Type }
  deriving (Eq, Show, Ord, Monoid)

instance Semigroup Subst where
  sa@(Subst a) <> sb@(Subst b) =
    Subst (fmap (apply sb) a <> fmap (apply sa) b)

instance HasSubst Type where
  apply (Subst e) (TyVar v) = Map.findWithDefault (TyVar v) v e
  apply d (a :-> b) = apply d a :-> apply d b
  apply _ x@TyCon{} = x

  ftv (TyVar v) = Set.singleton v
  ftv (a :-> b) = ftv a <> ftv b
  ftv TyCon{} = mempty

instance HasSubst Delta where
  apply s (Delta e) = Delta (fmap (apply s) e)
  ftv (Delta e) = foldMap ftv e

instance HasSubst a => HasSubst [a] where
  apply s = map (apply s)
  ftv = foldMap ftv

instance HasSubst Typing where
  apply s (Typing a d t) = Typing a (apply s d) (apply s t)
  ftv (Typing _ d t) = ftv d <> ftv t

instance HasSubst a => HasSubst (Spanned a) where
  apply s (Spanned sp x) = Spanned sp (apply s x)
  ftv (Spanned _ x) = ftv x

singleton :: Var -> Type -> Subst
singleton v t = Subst (Map.singleton v t)

substFromList :: [(Var, Type)] -> Subst
substFromList vs = Subst (Map.fromList vs)
