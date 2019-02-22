{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
module Syntax
  ( Exp(..)
  , Span(..)
  , Type(..)
  , Var(..)
  , Delta(..)
  , Typing(..)
  , Gamma(..)
  , typedAs, (!), (\-), insertGamma
  , expAnn, withTypeAnn
  ) where

import qualified Data.Map.Strict as Map

import Data.List
import Data.Char

import GHC.Exts

import Text.Parsec.Pos
import Text.Printf

data Span = Span SourcePos SourcePos
  deriving (Eq, Ord)

instance Show Span where
  show (Span a b) = printf "%s:%d:%d-%d:%d" (sourceName a) (sourceLine a) (sourceColumn a) (sourceLine b) (sourceColumn b)

instance Semigroup Span where
  Span a _ <> Span _ b = Span a b

newtype Var = Var { varName :: String }
  deriving (Eq, Ord)
  deriving newtype IsString

instance Show Var where
  show (Var v) = v

data Exp
  = Lam Span Var Exp
  | App Span Exp Exp
  | Use Span Var
  | Let Span (Var, Exp) Exp
  | Num Span Integer
  deriving (Eq, Ord)

expAnn :: Exp -> Span
expAnn (Lam a _ _) = a
expAnn (App a _ _) = a
expAnn (Use a _) = a
expAnn (Let a _ _) = a
expAnn (Num a _) = a

instance Show Exp where
  show (Lam _ v e) = "fun " ++ show v ++ " -> " ++ show e
  show (App _ l r) = showFun l ++ " " ++ showArg r where
    showFun x@Lam{} = paren (show x)
    showFun x@Let{} = paren (show x)
    showFun x = show x

    showArg x@App{} = paren (show x)
    showArg x = showFun x
  show (Use _ v) = show v
  show (Num _ x) = show x
  show (Let _ (a, b) x) =
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
  = Typing { typingAnn :: Maybe Span
           , typingEnv :: Delta
           , typingTy  :: Type }
  deriving (Eq, Ord)

instance Show Typing where
  show (Typing _ d t) = show d ++ " ⊢ " ++ show t

withTypeAnn :: Span -> Typing -> Typing
withTypeAnn a t = t { typingAnn = Just a }

typedAs :: Var -> Type -> Delta
typedAs v t = Delta (Map.singleton v t)

(!) :: Delta -> Var -> Maybe Type
Delta m ! v = Map.lookup v m

(\-) :: Delta -> Var -> Delta
Delta m \- v = Delta $ Map.delete v m

insertGamma :: Var -> Typing -> Gamma -> Gamma
insertGamma v t (Gamma g) = Gamma (Map.insert v t g)
