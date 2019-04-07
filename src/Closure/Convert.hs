{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Closure.Convert where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Control.Monad.Writer
import Control.Arrow

import qualified Closure.Lang as Cl
import Syntax

closureConvert :: Exp -> [Cl.Dec]
closureConvert c =
  let ((code, _), decs) = runWriter (convert [1..] c)
   in decs ++ [Cl.CodeMain code]

convert :: [Int] -> Exp -> Writer [Cl.Dec] (Cl.Exp, [Int])
convert (i:j:is) lam@(Lam _ var exp) = do
  let fv = Set.toList (free lam)
      env = Var ("env" ++ show i)
      cl = Var ("cl" ++ show j)

      sub = Map.fromList $
        zipWith (\var idx -> (var, Cl.Idx env idx)) fv [0..]

  (bd, is) <- fmap (first (Cl.substitute sub)) $
    convert is exp

  let code =
        Cl.CodeDec (length fv) cl (env, var) bd
      closure = Cl.Closure cl (map Cl.Ref fv)

  tell [code]
  pure (closure, is)

convert _ Lam{} = error "Not enough vars"

convert is (App _ f x) = do
  ~(f, j:js) <- convert is f
  let name = Var ("_" ++ show j)
      bind = (name, f)
  (x, ks) <- convert js x
  pure (Cl.Let bind $ Cl.App name x, ks)

convert is (Use _ v) = pure (Cl.Ref v, is)

convert is (Let _ (var, exp) body) = do
  (exp, js) <- convert is exp
  (body, ks) <- convert js body
  pure (Cl.Let (var, exp) body, ks)

convert is (Num _ x) = pure (Cl.Num x, is)
