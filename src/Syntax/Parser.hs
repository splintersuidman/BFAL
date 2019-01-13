{-# LANGUAGE NamedFieldPuns #-}

module Syntax.Parser
  ( parseProgram
  ) where

import Syntax.AST

import Comb

import Data.Char

parseProgram :: String -> Either String [Stmt]
parseProgram input =
  do (program, rest) <- maybe (Left "parse error") Right $ parse program input
     if null rest
        then Right ()
        else Left $ "parse error; not consumed: '" ++ rest ++ "'"
     return program

program :: Parser Char Maybe [Stmt]
program = many line

comments :: Parser Char Maybe ()
comments = between (many space) (many space) (many comment) `replaceWith` ()
  where
    comment = symbol '#' >> many (satisfy (/= '\n')) >> symbol '\n'

line :: Parser Char Maybe Stmt
line =
  do comments
     stmt <- stmt
     many space
     symbol ';'
     comments
     return stmt

stmt :: Parser Char Maybe Stmt
stmt =  stmtVar
    <|> stmtGoto
    <|> stmtSet
    <|> stmtWith
    <|> stmtPut
    <|> stmtRead
    <|> stmtIncr
    <|> stmtDecr
    <|> stmtAdd
    <|> stmtSub

stmtVar :: Parser Char Maybe Stmt
stmtVar =
  do token "var"
     many1 space
     name <- ident
     initValue <- init
     return SVar { _name = name, _initValue = initValue }
  where
    init = option (Just <$> (many space >> symbol '=' >> many space >> expr)) Nothing

stmtGoto :: Parser Char Maybe Stmt
stmtGoto =
  do token "goto"
     many1 space
     name <- ident
     return SGoto { _name = name }

stmtSet :: Parser Char Maybe Stmt
stmtSet =
  do token "set"
     many1 space
     value <- expr
     return SSet { _value = value }

stmtWith :: Parser Char Maybe Stmt
stmtWith =
  do name <- ident
     many space
     symbol '~'
     many space
     stmt <- stmt
     return SWith { _name = name, _stmt = stmt }

stmtPut :: Parser Char Maybe Stmt
stmtPut = token "put" `replaceWith` SPut

stmtRead :: Parser Char Maybe Stmt
stmtRead = token "read" `replaceWith` SRead

stmtIncr :: Parser Char Maybe Stmt
stmtIncr = token "incr" `replaceWith` SIncr

stmtDecr :: Parser Char Maybe Stmt
stmtDecr = token "decr" `replaceWith` SDecr

stmtClear :: Parser Char Maybe Stmt
stmtClear = token "clear" `replaceWith` SClear

stmtAdd :: Parser Char Maybe Stmt
stmtAdd =
  do token "add"
     many1 space
     value <- expr
     return SAdd { _value = value }

stmtSub :: Parser Char Maybe Stmt
stmtSub =
  do token "sub"
     many1 space
     value <- expr
     return SSub { _value = value }

expr :: Parser Char Maybe Expr
expr = exprInt

exprInt :: Parser Char Maybe Expr
exprInt = EInt <$> int

ident :: Parser Char Maybe Ident
ident =
  do first <- lowercase <|> uppercase
     rest  <- many (lowercase <|> uppercase <|> digit)
     return (first : rest)
