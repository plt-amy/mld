module Closure.ToC where

import Control.Monad.State

import Closure.Lang
import Syntax (Var)

import Text.Printf
import Data.List

progToC :: [Dec] -> String
progToC ds = unlines (inc:evalState (traverse decToC ds) 0) where
  inc = "#include \"rt.h\""

decToC :: Dec -> State Int String
decToC (CodeDec free name (env, arg) body) = do
  code <- expToC returnReturn body

  pure $
    unlines [ printf "struct val *%s (struct val *%s[%d], struct val *%s) {"
                (show name) (show env) free (show arg)
            , code
            , "}"
            ]
decToC (CodeMain body) = do
  code <- expToC dumpReturn body

  pure $
    unlines [ "int main(void) {"
            , code
            , "}"
            ]

expToC :: Return -> Exp -> State Int String
expToC ret (App f x) = do
  arg <- expToC expReturn x
  let var = show f

  ret $ printf "%s->c.code(%s->c.env, %s)" var var arg
expToC ret (Let (var, bind) body) = do
  let binder =
        bindReturn var
  code <- expToC binder bind
  cont <- expToC ret body
  pure (unlines [ "struct val *" ++ show var ++ ";", code, cont ])
expToC ret (Num x) = ret $ "mk_int(" ++ show x ++ ")"
expToC ret (Ref v) = ret (show v)
expToC ret (Closure name env) = do
  bindcode <- traverse (expToC expReturn) env
  let binds =
        intercalate ", " bindcode
  ret $
    printf "mk_closure(%s, (struct val *[%d]){ %s })"
      (show name) (length env) binds
expToC ret (Idx v i) = ret $ show v ++ "[" ++ show i ++ "]"

type Return = String -> State Int String

bindReturn :: Var -> Return
bindReturn v x =
  pure $ show v ++ " = " ++ x ++ ";"

returnReturn, expReturn, dumpReturn :: Return
returnReturn x = pure $ "return " ++ x ++ ";"
expReturn x = pure x
dumpReturn x = pure $ "dump(" ++ x ++ ");"
