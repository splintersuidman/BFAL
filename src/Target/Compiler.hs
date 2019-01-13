{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Target.Compiler
  ( compileProgram
  ) where

import Syntax.AST
import Syntax.Parser
import Target.Brainfuck

import Comb

import           Control.Monad (foldM)
import           Control.Lens
import           Data.Char (ord)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

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
     return $ view output compiler

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
                 Just value -> compileExpr value
                 -- Do not transform the compiler if no initValue is given.
                 Nothing -> Right . id
     in case compilerLookup name c of
          Right _ -> Left $ "symbol '" ++ name ++ "' already exists"
          Left _ -> init . compilerAddVar name $ c
-- Goto a variable if it exists.
compile SGoto { _name = name } c
  = compilerGoToSymbol name c
-- Set the current variable to the given value.
compile SSet { _value = value } c
  = compileExpr value . compilerClearCell $ c
compile SWith { _name = name, _stmt = stmt } c
  = compilerGoToSymbol name c >>= compile stmt
-- Print the current cell.
compile SPut c
  = Right . over output (++ [BFPrint]) $ c
-- Read a character into the current cell.
compile SRead c
  = Right . over output (++ [BFRead]) $ c
-- Increment the current cell.
compile SIncr c
  = Right . over output (++ [BFIncrement]) $ c
-- Decrement the current cell.
compile SDecr c
  = Right . over output (++ [BFDecrement]) $ c
-- Clear the current cell.
compile SClear c
  = Right . compilerClearCell $ c
-- Add a value to the current cell.
compile SAdd { _value = value } c
  = case value of
      EInt value -> Right . over output (++ (take value $ repeat BFIncrement)) $ c
-- Subtract a value from the current cell.
compile SSub { _value = value } c
  = case value of
      EInt value -> Right . over output (++ (take value $ repeat BFDecrement)) $ c

-- Compile an expression.
compileExpr :: Expr -> Compiler -> Either String Compiler
compileExpr (EInt value)
  = Right . over output (++ (take value $ repeat BFIncrement))

-- Move the compiler to a symbol.
compilerGoToSymbol :: Ident -> Compiler -> Either String Compiler
compilerGoToSymbol name c
  = case compilerLookup name c of
      Left err -> Left $ "cannot go to symbol '" ++ name ++ "': " ++ err
      Right symbol -> Right $ compilerGoTo (view cell symbol) c

-- Move the compiler to a cell.
compilerGoTo :: Int -> Compiler -> Compiler
compilerGoTo cell c
  = let instructions =
          if cell < view cellPointer c
             then take (view cellPointer c - cell) $ repeat BFPrevious
             else take (cell - view cellPointer c) $ repeat BFNext
     in set cellPointer cell . over output (++ instructions) $ c

-- Look up a symbol in the symbols map.
compilerLookup :: Ident -> Compiler -> Either String Symbol
compilerLookup name c
  = maybe
    (Left $ "symbol '" ++ name ++ "' not found")
    Right
    (Map.lookup name $ view symbols c)

-- Add a variable.
compilerAddVar :: Ident -> Compiler -> Compiler
compilerAddVar name c
  = over nextCell (+ 1)
  . (\c -> over symbols (Map.insert name Symbol { _cell = view nextCell c }) c)
  . compilerGoTo (view nextCell c)
  $ c

-- Clear the current cell.
-- A cell can be cleared with @[-]@.
compilerClearCell :: Compiler -> Compiler
compilerClearCell = over output (++ [BFJumpForward, BFDecrement, BFJumpBack])
