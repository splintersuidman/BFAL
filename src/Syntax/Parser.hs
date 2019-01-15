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
comments =
  do many space
     many (
       do comment
          many space
       )
     return ()
  where
    comment =
      do symbol '#'
         many (satisfy (/= '\n'))
         symbol '\n' `replaceWith` () <|> eof

-- many space >> (comment >> many space >> (comment >> many space)?)?

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
    <|> stmtSet
    <|> stmtPut
    <|> stmtRead
    <|> stmtCopy
    <|> stmtWhile
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
    init = option (Just <$> (many1 space >> expr)) Nothing

stmtSet :: Parser Char Maybe Stmt
stmtSet =
  do token "set"
     many1 space
     name <- ident
     many1 space
     value <- expr
     return SSet { _name = name, _value = value }

stmtPut :: Parser Char Maybe Stmt
stmtPut =
  do token "put"
     many1 space
     name <- ident
     return SPut { _name = name }

stmtRead :: Parser Char Maybe Stmt
stmtRead =
  do token "read"
     many1 space
     name <- ident
     return SRead { _name = name }

stmtCopy :: Parser Char Maybe Stmt
stmtCopy =
  do token "copy"
     many1 space
     from <- ident
     many1 space
     to <- ident
     return SCopy { _from = from, _to = to }

stmtWhile :: Parser Char Maybe Stmt
stmtWhile =
  do token "while"
     many1 space
     name <- ident
     many space
     body <- block
     return SWhile { _name = name, _body = body }

stmtIncr :: Parser Char Maybe Stmt
stmtIncr =
  do token "incr"
     many1 space
     name <- ident
     return SIncr { _name = name }

stmtDecr :: Parser Char Maybe Stmt
stmtDecr =
  do token "decr"
     many1 space
     name <- ident
     return SDecr { _name = name }

stmtClear :: Parser Char Maybe Stmt
stmtClear =
  do token "clear"
     many1 space
     name <- ident
     return SClear { _name = name }

stmtAdd :: Parser Char Maybe Stmt
stmtAdd =
  do token "add"
     many1 space
     name <- ident
     many1 space
     value <- expr
     return SAdd { _name = name, _value = value }

stmtSub :: Parser Char Maybe Stmt
stmtSub =
  do token "sub"
     many1 space
     name <- ident
     many1 space
     value <- expr
     return SSub { _name = name, _value = value }

expr :: Parser Char Maybe Expr
expr = exprInt

exprInt :: Parser Char Maybe Expr
exprInt = EInt <$> int

block :: Parser Char Maybe [Stmt]
block =
  do symbol '{'
     stmts <- program
     symbol '}'
     return stmts

ident :: Parser Char Maybe Ident
ident =
  do first <- lowercase <|> uppercase
     rest  <- many (lowercase <|> uppercase <|> digit)
     return $ IName (first : rest)
