{-# LANGUAGE DeriveFunctor, MultiParamTypeClasses #-}
module Typing.Monad 
  ( TypingM
  , local
  , newVar
  , applySub
  , extendSub
  , runTyping
  , getTyping
  , refresh
  , module Control.Monad.Except
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Control.Monad.Except
import Control.Applicative

import Syntax.Subst
import Syntax

import Typing.Errors

newtype TypingM a =
  TypingM { runM :: [Var] -> Gamma -> Subst -> Either TypeError (a, [Var], Subst) }
  deriving Functor

varSupply :: [Var]
varSupply = map Var $ [1..] >>= flip replicateM ['a'..'z']

instance Applicative TypingM where
  pure x = TypingM $ \vars _ sub -> pure (x, vars, sub)
  liftA2 f (TypingM ka) (TypingM kb) = TypingM $ \vars gamma sub -> do
    (a, vars', sub') <- ka vars gamma sub
    (b, vars'', sub'') <- kb vars' gamma sub'
    pure (f a b, vars'', sub'')

instance Monad TypingM where
  TypingM k >>= f = TypingM $ \vars gamma sub -> do
    (x, vars', sub') <- k vars gamma sub
    runM (f x) vars' gamma sub'

local :: (Gamma -> Gamma) -> TypingM a -> TypingM a
local cont (TypingM k) = TypingM $ \vars gamma sub ->
  k vars (cont gamma) sub

newVar :: TypingM Var
newVar = TypingM $ \(x:xs) _ sub -> pure (x, xs, sub)

applySub :: HasSubst t => t -> TypingM t
applySub tau = TypingM $ \vars _ sub -> pure (apply sub tau, vars, sub)

extendSub :: Subst -> TypingM ()
extendSub sub = TypingM $ \vars _ sub' -> pure ((), vars, sub <> sub')

getTyping :: Var -> TypingM (Maybe Typing)
getTyping v = TypingM $ \vars (Gamma env) sub -> pure (Map.lookup v env, vars, sub)

instance MonadError TypeError TypingM where
  throwError e = TypingM $ \_ _ sub -> Left (apply sub e)
  catchError (TypingM k) catch = TypingM $ \vars gamma sub ->
    case k vars gamma sub of
      Left e -> runM (catch e) vars gamma sub
      Right x -> pure x

runTyping :: Gamma -> TypingM a -> Either TypeError a
runTyping env (TypingM k) =
  case k varSupply env mempty of
    Left e -> Left e
    Right (x, _, _) -> pure x

refresh :: Span -> Typing -> TypingM Typing
refresh a (Typing _ delta tau) = TypingM $ \vars _ sub ->
  let tau_fv = Set.toList (ftv tau `Set.difference` foldMap ftv (getDelta delta))
      (used, vars') = splitAt (length tau_fv) vars
      sub' = substFromList (zip tau_fv (map TyVar used))
   in pure (apply (sub <> sub') (Typing (Just a) delta tau), vars', sub)
