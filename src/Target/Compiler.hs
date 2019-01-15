{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Target.Compiler where
  -- ( compileProgram
  -- ) where

import Syntax.AST
import Syntax.Parser
import Target.Brainfuck

import Comb

import           Control.Monad (foldM)
import           Control.Lens hiding ((|>))
import           Data.Char (ord)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

(|>) :: (a -> b) -> (b -> c) -> a -> c
(|>) = flip (.)

data Symbol
  = Symbol
    { _cell :: Int -- The cell which is occupied by the symbol.
    }
    deriving (Show)
makeLenses ''Symbol

data Compiler
  = Compiler
    { _cellPointer :: Int -- The current cell.
    , _nextCell    :: Int -- The next unoccupied cell.
    , _symbols     :: Map Ident Symbol
    , _output      :: Brainfuck
    }
    deriving (Show)
makeLenses ''Compiler

-- Compile a program and return the output.
compileProgram :: String -> Either String Brainfuck
compileProgram input =
  do program <- parseProgram input
     compiler <- foldM (flip compile) compilerNew program
     return $ compiler^.output

compilerNew :: Compiler
compilerNew = Compiler
  { _cellPointer = 0
  , _nextCell    = 0
  , _symbols     = Map.empty
  , _output      = []
  }

-- Compile a statement.
compile :: Stmt -> Compiler -> Either String Compiler
-- Create a new variable if it does not yet exist.
compile SVar { _name = name, _initValue = initValue } c
  = let init = case initValue of
                 -- Go to the newly added symbol and compile the value.
                 Just value -> (\c -> compilerGoToSymbol name c >>= compileExpr value)
                 -- Do not transform the compiler if no initValue is given.
                 Nothing -> return
     in case compilerLookup name c of
          Right _ -> Left $ "symbol '" ++ show name ++ "' already exists"
          Left _ -> init . compilerAddVar name $ c
-- Set a cell to the given value.
compile SSet { _name = name, _value = value } c
  =   compilerGoToSymbol name c
  >>= compileExpr value . compilerClearCell
-- Print a cell.
compile SPut { _name = name } c
  =   compilerGoToSymbol name c
  >>= return . over output (++ [BFPrint])
-- Read a character into a cell.
compile SRead { _name = name } c
  =   compilerGoToSymbol name c
  >>= return . over output (++ [BFRead])
-- Copy a cell to another cell.
compile SCopy { _from = from, _to = to } c
  = let temp = ICell $ c^.nextCell
     in foldM (flip compile) c
        [ SWhile
          { _name = from
          , _body =
            [ SIncr { _name = to }
            , SIncr { _name = temp }
            , SDecr { _name = from }
            ]
          }
        , SWhile
          { _name = temp
          , _body =
            [ SIncr { _name = from }
            , SDecr { _name = temp }
            ]
          }
        ]
compile SWhile { _name = name, _body = body } c
  =   compilerGoToSymbol name c
  >>= return . over output (++ [BFJumpForward])
  >>= (\c -> foldM (flip compile) c body)
  >>= compilerGoToSymbol name
  >>= return . over output (++ [BFJumpBack])
-- Increment a cell.
compile SIncr { _name = name } c
  =   compilerGoToSymbol name c
  >>= return . over output (++ [BFIncrement])
-- Decrement a cell.
compile SDecr { _name = name } c
  =   compilerGoToSymbol name c
  >>= return . over output (++ [BFDecrement])
-- Clear a cell.
compile SClear { _name = name } c
  =   compilerGoToSymbol name c
  >>= return . compilerClearCell
-- Add a value to a cell.
compile SAdd { _name = name, _value = value } c
  = case value of
      EInt value -> compilerGoToSymbol name c
                >>= return . over output (++ (take value $ repeat BFIncrement))
-- Subtract a value from a cell.
compile SSub { _name = name, _value = value } c
  = case value of
      EInt value -> compilerGoToSymbol name c
                >>= return . over output (++ (take value $ repeat BFDecrement))
-- Compile an expression.
compileExpr :: Expr -> Compiler -> Either String Compiler
compileExpr (EInt value)
  = Right . over output (++ (take value $ repeat BFIncrement))

-- Move the compiler to a symbol.
compilerGoToSymbol :: Ident -> Compiler -> Either String Compiler
compilerGoToSymbol name c
  = case compilerLookup name c of
      Left err -> Left $ "cannot go to symbol '" ++ show name ++ "': " ++ err
      Right symbol -> Right $ compilerGoTo (symbol^.cell) c

-- Move the compiler to a cell.
compilerGoTo :: Int -> Compiler -> Compiler
compilerGoTo cell c
  = let instructions =
          if cell < c^.cellPointer
             then take (c^.cellPointer  - cell) $ repeat BFPrevious
             else take (cell - c^.cellPointer ) $ repeat BFNext
     in set cellPointer cell . over output (++ instructions) $ c

-- Look up a symbol in the symbols map.
compilerLookup :: Ident -> Compiler -> Either String Symbol
compilerLookup (ICell cell) c
  = Right Symbol { _cell = cell }
compilerLookup name c
  = maybe
    (Left $ "symbol '" ++ show name ++ "' not found")
    Right
    (Map.lookup name $ c^.symbols)

-- Add a variable.
compilerAddVar :: Ident -> Compiler -> Compiler
compilerAddVar name c
  = over nextCell (+ 1)
  . (\c -> over symbols (Map.insert name Symbol { _cell = c^.nextCell }) c)
  $ c

-- Clear the current cell.
-- A cell can be cleared with @[-]@.
compilerClearCell :: Compiler -> Compiler
compilerClearCell = over output (++ [BFJumpForward, BFDecrement, BFJumpBack])
