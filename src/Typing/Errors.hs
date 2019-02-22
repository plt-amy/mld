module Typing.Errors
  ( TypeError(..)
  , addMergeErrorCtx
  , addErrorSpan
  ) where

import Syntax

data TypeError
  = OccursError Var Type
  | NotEqualError Type Type
  | MergeError Var TypeError
  | SpannedError Span TypeError

instance Show TypeError where
  show (OccursError v t) =
    "Variable " ++ show v ++ " occurs in type " ++ show t
  show (NotEqualError a b) =
    "Types " ++ show a ++ " and " ++ show b ++ " aren't compatible"
  show (MergeError v e) =
    show e ++ "\nWhen merging type of " ++ show v
  show (SpannedError s e) = show s ++ ": " ++ show e


addMergeErrorCtx :: Var -> TypeError -> TypeError
addMergeErrorCtx v (SpannedError s e) = SpannedError s $ addMergeErrorCtx v e
addMergeErrorCtx v e = MergeError v e

addErrorSpan :: Span -> TypeError -> TypeError
addErrorSpan _ e@SpannedError{} = e
addErrorSpan s e = SpannedError s e
