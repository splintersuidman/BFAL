module Syntax.AST
  ( Ident (..)
  , Stmt (..)
  , Expr (..)
  ) where

-- Grammar:
--   Program ::= (Line)*
--   Line    ::= (Comment)* Stmt ";" (Comment)*
--   Comment ::= "#" (not "\n")* "\n" | whitespace
--   Stmt    ::= SVar | SSet | SPut | SIncr | SDecr | SAdd | SSub
--   SVar    ::= "var" Ident (Expr)?
--   SSet    ::= "set" Ident Expr
--   SPut    ::= "put" Ident
--   SRead   ::= "read" Ident
--   SCopy   ::= "copy" Ident Ident
--   SWhile  ::= "while" Ident Block
--   SIncr   ::= "incr" Ident
--   SDecr   ::= "decr" Ident
--   SClear  ::= "clear" Ident
--   SAdd    ::= "add" Ident Expr
--   SSub    ::= "sub" Ident Expr
--   Expr    ::= EInt
--   EInt    ::= (digit)+
--   Block   ::= "{" Program "}"
--   Ident   ::= (lowercase | uppercase)(lowercase | uppercase | digit)*

data Stmt
  = SVar
    { _name      :: Ident
    , _initValue :: Maybe Expr
    }
  | SSet
    { _name  :: Ident
    , _value :: Expr
    }
  | SPut
    { _name :: Ident
    }
  | SRead
    { _name :: Ident
    }
  | SCopy
    { _from :: Ident
    , _to   :: Ident
    }
  | SWhile
    { _name :: Ident
    , _body :: [Stmt]
    }
  | SIncr
    { _name :: Ident
    }
  | SDecr
    { _name :: Ident
    }
  | SClear
    { _name :: Ident
    }
  | SAdd
    { _name  :: Ident
    , _value :: Expr
    }
  | SSub
    { _name  :: Ident
    , _value :: Expr
    }
  deriving (Show)

data Expr
  = EInt Int -- NOTE: an integer may not (yet) be negative.
  deriving (Show)

data Ident
  = IName String
  | ICell Int
  deriving (Eq, Ord)

instance Show Ident where
  show (IName name) = name
  show (ICell cell) = "<cell at " ++ show cell ++ ">"
