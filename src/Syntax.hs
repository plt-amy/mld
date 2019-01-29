{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
module Syntax
  ( Exp(..)
  , Type(..)
  , Var(..)
  , Delta(..)
  , Typing(..)
  , Gamma(..)
  , typedAs, (⊢), (!), (\-), insertGamma
  ) where

import qualified Data.Map.Strict as Map

import Data.List
import Data.Char

import GHC.Exts


newtype Var = Var { varName :: String }
  deriving (Eq, Ord)
  deriving newtype IsString

instance Show Var where
  show (Var v) = v

data Exp
  = Lam Var Exp
  | App Exp Exp
  | Use Var
  | Let (Var, Exp) Exp
  | Num Integer
  deriving (Eq, Ord)

instance Show Exp where
  show (Lam v e) = "fun " ++ show v ++ " -> " ++ show e
  show (App l r) = showFun l ++ " " ++ showArg r where
    showFun x@Lam{} = paren (show x)
    showFun x@Let{} = paren (show x)
    showFun x = show x

    showArg x@App{} = paren (show x)
    showArg x = showFun x
  show (Use v) = show v
  show (Num x) = show x
  show (Let (a, b) x) =
    "let " ++ show a ++ " = " ++ show b ++ " in " ++ show x

paren :: String -> String
paren x = "(" ++ x ++ ")"

newtype Delta = Delta { getDelta :: Map.Map Var Type }
  deriving (Eq, Ord, Semigroup, Monoid)

instance Show Delta where
  show (Delta xs)
    | Map.null xs = "{}"
    | otherwise
    = "{ "
   ++ intercalate ", "
        (map (\(a, b) -> show a ++ " :: " ++ show b) (Map.toList xs))
   ++ " }"

newtype Gamma = Gamma { getGamma :: Map.Map Var Typing }
  deriving (Eq, Show, Ord, Semigroup, Monoid)

data Type
  = TyVar Var
  | Type :-> Type
  | TyCon Var
  deriving (Eq, Ord)

instance IsString Type where
  fromString [] = error "empty type"
  fromString xs@(x:_)
    | isUpper x = TyCon (fromString xs)
    | otherwise = TyVar (fromString xs)

infixr 1 :->

instance Show Type where
  show (TyVar (Var v)) = v
  show (TyCon (Var v)) = v
  show (l :-> t) = l' ++ " -> " ++ show t where
    l' = case l of
      (:->){} -> paren (show l)
      _ -> show l

data Typing
  = Typing { typingEnv :: Delta
           , typingTy :: Type }
  deriving (Eq, Ord)

instance Show Typing where
  show (Typing d t) = show d ++ " ⊢ " ++ show t

typedAs :: Var -> Type -> Delta
typedAs v t = Delta (Map.singleton v t)

(⊢) :: Delta -> Type -> Typing
(⊢) = Typing

(!) :: Delta -> Var -> Maybe Type
Delta m ! v = Map.lookup v m

(\-) :: Delta -> Var -> Delta
Delta m \- v = Delta $ Map.delete v m

insertGamma :: Var -> Typing -> Gamma -> Gamma
insertGamma v t (Gamma g) = Gamma (Map.insert v t g)
