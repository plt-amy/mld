module Parser (parser, ParseError) where

import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language
import Text.Parsec

import Syntax

parser :: String -> Either ParseError Exp
parser = parse expr "<input>" where
  lexer = Tok.makeTokenParser style where
    ops = ["->","\\","+","*","-","="]
    names = [ "fun", "let", "in" ]
    style = haskellStyle { Tok.reservedOpNames = ops,
                           Tok.reservedNames = names,
                           Tok.commentLine = "#" }

  lambda = do
    Tok.reserved lexer "fun"
    args <- many1 (Tok.identifier lexer)
    Tok.reservedOp lexer "->"
    body <- expr
    return $ foldr Lam body (map Var args)

  let' = do
    Tok.reserved lexer "let"
    name <- Tok.identifier lexer
    args <- many (Tok.identifier lexer)
    Tok.reservedOp lexer "="
    body <- flip (foldr Lam) (map Var args) <$> expr
    Tok.reserved lexer "in"
    Let (Var name, body) <$> expr


  term =  Tok.parens lexer expr
      <|> (Use . Var <$> Tok.identifier lexer)
      <|> (Num <$> Tok.natural lexer)
      <|> lambda
      <|> let'

  expr = do
    es <- many1 term
    return (foldl1 App es)
