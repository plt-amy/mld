{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map.Strict as Map

import System.Console.Haskeline

import Parser
import Syntax

import Typing.Monad
import Typing.Errors
import Typing

import Data.Char

realise :: String -> Either (Either ParseError TypeError) Typing
realise code = do
  expr <- mapLeft Left (parser code)
  ty <-
    mapLeft Right (runTyping demonstration (infer expr))
  pure ty

demonstration :: Gamma
demonstration =
  Gamma . Map.fromList $
    [ (Var "add", poly $ "Int" :-> "Int" :-> "Int")
    , (Var "if", poly $ "Bool" :-> "a" :-> "a" :-> "a")
    ]
  where poly = Typing mempty

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right

main :: IO ()
main = runInputT defaultSettings loop where
  loop :: InputT IO ()
  loop = do
    minput <- getInputLine "⊢ "
    case minput of
      Nothing -> return ()
      Just "quit" -> return ()
      Just input -> do
        liftIO (interp input)
        loop

  interp :: String -> IO ()
  interp s =
    case realise s of
      Left e -> 
        let print' = putStrLn . reverse . dropWhile isSpace . reverse . show
         in either print print' e
      Right x -> print x

