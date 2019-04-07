module Closure.Lang where

import qualified Data.Map.Strict as Map
import Data.Map (Map)

import Syntax (Var(..))

data Exp
  = App Var Exp
  | Closure Var [Exp]
  | Let (Var, Exp) Exp
  | Num Integer

  | Ref Var
  | Idx Var Integer
  deriving (Eq, Show)

data Dec
  = CodeDec !Int Var (Var, Var) Exp
  | CodeMain Exp
  deriving (Eq, Show)

substitute :: Map Var Exp -> Exp -> Exp
substitute mp ex@(Ref v)
  | Just x <- Map.lookup v mp = x
  | otherwise = ex

substitute mp (Let (v, e) b) =
  Let (v, substitute mp e) (substitute (Map.delete v mp) b)

substitute mp (Closure v ex) =
  Closure v (map (substitute mp) ex)

substitute mp (App f x) = App f (substitute mp x)
substitute _ x@(Num _) = x
