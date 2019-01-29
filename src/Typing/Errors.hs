module Typing.Errors
  ( TypeError
  , occursError
  , notEqualError
  , addMergeErrorCtx
  ) where

import Syntax

newtype TypeError = TypeError String

instance Show TypeError where
  show (TypeError x) = x

occursError :: Var -> Type -> TypeError
occursError v t = TypeError $
  "Variable " ++ show v ++ " occurs in type " ++ show t

notEqualError :: Type -> Type -> TypeError
notEqualError a b = TypeError $
  "Types " ++ show a ++ " and " ++ show b ++ " aren't compatible"

addMergeErrorCtx :: Var -> TypeError -> TypeError
addMergeErrorCtx v (TypeError t) = TypeError $
  unlines [ t, "When merging type of " ++ show v ]
