{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Typing.Errors
  ( TypeError, showError
  , addMergeErrorCtx
  , notEqualError
  , occursError
  , ErrorFragment(..)
  , withNote, noted
  ) where

import Control.Monad.Except
import Text.Parsec.Pos

import Data.Maybe
import GHC.Exts
import Syntax

import Syntax.Subst

newtype TypeError = TypeError [ErrorFragment]
  deriving (Eq, Show, Semigroup, Monoid, HasSubst)

data ErrorFragment
  = Line String
  | Snippet (Maybe Span) Type
  deriving (Eq, Show)

instance HasSubst ErrorFragment where
  apply _ x@Line{} = x
  apply s (Snippet sp t) = Snippet sp (apply s t)

  ftv Line{} = mempty
  ftv (Snippet _ t) = ftv t


instance IsString ErrorFragment where
  fromString = Line

addMergeErrorCtx :: Var -> Spanned Type -> Spanned Type
                 -> TypeError -> TypeError
addMergeErrorCtx var (Spanned use_a tau) (Spanned use_b tau') t =
  t <> TypeError
    [ Line $ "The variable " ++ show var ++ " has inconsistent uses:"
    , Snippet (Just use_a) tau
    , Line "and"
    , Snippet (Just use_b) tau'
    ]

occursError :: Var -> Type -> TypeError
occursError v t =
  TypeError [
    Line $ "The type variable " ++ show v ++ " occurs in " ++ show t
  ]

notEqualError :: Type -> Type -> TypeError
notEqualError l r =
  TypeError [
    Line $ "Couldn't match types " ++ show l ++ " and " ++ show r
  ]

withNote :: TypeError -> [ErrorFragment] -> TypeError
withNote t ts = t <> TypeError ts

noted :: MonadError TypeError m => [ErrorFragment] -> TypeError -> m ()
noted = (throwError .) . flip withNote

showError :: String -> TypeError -> String
showError source (TypeError frag) = unlines (mapMaybe showFrag frag) where
  showFrag (Line l) = Just l
  showFrag (Snippet Nothing _) = Nothing
  showFrag (Snippet (Just (Span from to)) comment) =
    let line = sourceLines !! (sourceLine from - 1)
        skip = sourceColumn from - 1
        len = sourceColumn to - sourceColumn from
     in Just $ cat [ line
                   , replicate skip ' ' ++ "\x1b[1;35m"
                       ++ replicate len '^'
                       ++ " \x1b[0m" ++ show comment]

  sourceLines = lines source
  cat = unlines' . map ("  " ++)
  unlines' xs = unlines (init xs) ++ last xs

