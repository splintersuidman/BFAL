module Target.Brainfuck
  ( BFInstruction (..)
  , Brainfuck
  ) where

data BFInstruction
  -- @+@
  = BFIncrement
  -- @-@
  | BFDecrement
  -- @>@
  | BFNext
  -- @<@
  | BFPrevious
  -- @.@
  | BFPrint
  -- @,@
  | BFRead
  -- @[@
  | BFJumpForward
  -- @]@
  | BFJumpBack

instance Show BFInstruction where
  show BFIncrement   = "+"
  show BFDecrement   = "-"
  show BFNext        = ">"
  show BFPrevious    = "<"
  show BFPrint       = "."
  show BFRead        = ","
  show BFJumpForward = "["
  show BFJumpBack    = "]"

type Brainfuck = [BFInstruction]
