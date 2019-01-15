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
          Right _ -> Left $ "symbol '" ++ name ++ "' already exists"
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
-- TODO: Add a while-statement. The code can then be translated from an abstract
-- syntax tree representing the copy algorithm.
compile SCopy { _from = from, _to = to } c
  = let tempCell = c^.nextCell in
    do Symbol { _cell = from } <- compilerLookup from c
       Symbol { _cell = to } <- compilerLookup to c
       return
         (  -- # Copy `from` to `to` and `temp`, but clear `from`.
            -- goto from;
            compilerGoTo from
            -- while from {
         |> over output (++ [BFJumpForward])
            --   goto to;
         |> compilerGoTo to
            --   incr to;
         |> over output (++ [BFIncrement])
            --   goto temp;
         |> compilerGoTo tempCell
            --   incr temp;
         |> over output (++ [BFIncrement])
            --   goto from;
         |> compilerGoTo from
            --   decr from;
         |> over output (++ [BFDecrement])
            -- } # Close while-loop.
         |> over output (++ [BFJumpBack])
            -- # Move `temp` to `from`, while clearing `temp`.
            -- goto temp;
         |> compilerGoTo tempCell
            -- while temp {
         |> over output (++ [BFJumpForward])
            --   goto from;
         |> compilerGoTo from
            --   incr from;
         |> over output (++ [BFIncrement])
            --   goto temp;
         |> compilerGoTo tempCell
            --   decr temp;
         |> over output (++ [BFDecrement])
            -- } # Close while-loop.
         |> over output (++ [BFJumpBack])
         $  c
         )
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
      Left err -> Left $ "cannot go to symbol '" ++ name ++ "': " ++ err
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
compilerLookup name c
  = maybe
    (Left $ "symbol '" ++ name ++ "' not found")
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
