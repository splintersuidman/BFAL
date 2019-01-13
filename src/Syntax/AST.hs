module Syntax.AST
  ( Ident
  , Stmt (..)
  , Expr (..)
  ) where

-- Grammar:
--   Program ::= (Line)*
--   Line    ::= Stmt ";"
--   Stmt    ::= SVar | SGoto | SSet | SWith | SPut | SIncr | SDecr | SAdd | SSub
--   SVar    ::= "var" Ident ("=" Expr)?
--   SGoto   ::= "goto" Ident
--   SSet    ::= "set" Expr
--   SWith   ::= Ident "~" Stmt
--   SPut    ::= "put"
--   SRead   ::= "read"
--   SIncr   ::= "incr"
--   SDecr   ::= "decr"
--   SClear  ::= "clear"
--   SAdd    ::= "add" Expr
--   SSub    ::= "sub" Expr
--   Expr    ::= EInt
--   EInt    ::= (digit)+
--   Ident   ::= (lowercase | uppercase)(lowercase | uppercase | digit)*

data Stmt
  = SVar
    { _name      :: Ident
    , _initValue :: Maybe Expr
    }
  | SGoto
    { _name :: Ident
    }
  | SSet
    { _value :: Expr
    }
  | SWith
    { _name :: Ident
    , _stmt :: Stmt
    }
  | SPut
  | SRead
  | SIncr
  | SDecr
  | SClear
  | SAdd
    { _value :: Expr
    }
  | SSub
    { _value :: Expr
    }
  deriving (Show)

data Expr
  = EInt Int -- NOTE: an integer may not (yet) be negative.
  deriving (Show)

type Ident = String
