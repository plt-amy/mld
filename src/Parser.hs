module Parser
  ( parser
  , ParseError
  , Span) where

import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language
import Text.Parsec

import Syntax

parser :: String -> Either ParseError Exp
parser = parse (expr <* eof) "<input>" where
  lexer = Tok.makeTokenParser style where
    ops = ["->","\\","+","*","-","="]
    names = [ "fun", "let", "in" ]
    style = haskellStyle { Tok.reservedOpNames = ops,
                           Tok.reservedNames = names,
                           Tok.commentLine = "#" }

  positioned ps = do
    s <- getPosition
    x <- ps
    e <- getPosition
    pure $ x (Span s e)

  lambda = positioned $ do
    Tok.reserved lexer "fun"
    args <- many1 (Tok.identifier lexer)
    Tok.reservedOp lexer "->"
    body <- expr
    pure $ \p -> foldr (Lam p . Var) body args

  let' = positioned $ do
    Tok.reserved lexer "let"
    name <- Tok.identifier lexer
    body <- positioned $ do
      args <- many (Tok.identifier lexer)
      Tok.reservedOp lexer "="
      body <- expr
      pure $ \p -> foldr (Lam p . Var) body args

    Tok.reserved lexer "in"
    rest <- expr
    pure $ \p -> Let p (Var name, body) rest


  term =  Tok.parens lexer expr
      <|> positioned (flip Use . Var <$> Tok.identifier lexer)
      <|> positioned (flip Num <$> Tok.natural lexer)
      <|> lambda
      <|> let'

  expr = do
    es <- many1 term
    return (foldl1 mkApp es)

  mkApp l r = App (expAnn l <> expAnn r) l r
