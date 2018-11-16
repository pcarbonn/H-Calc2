module Interpreter.Interpreter where

  -- this module is the main entry point of the interpreter

  import Interpreter.A_Nucleus
  import Interpreter.B_Add
  -- import Interpreter.C_Mul
  -- import Interpreter.Transfos

  import Data.OpenADT
  -- import Fmt
  -- import Text.Megaparsec
  -- import Text.Megaparsec.Char as M
  -- import Text.Show

  -- row-types
  import           Data.Row


  -- main parser
  --------------------------------------------------------




  -- evaluation
  --------------------------------------------------------

  data Result
    = RInt Int
    | RFloat Float
    | RError Text
    deriving (Show, Eq)



  -- type specialisation
  --------------------------------------------------------

  -- type AST2 = EADT '[HErrorF, EmptyNoteF, ValF, FloatValF, AddF, MulF, TypF]
  type AST1F = AST0F .+ ("typF"       .== TypF)
  type AST0F  = (("hErrorF"    .== HErrorF)
              .+ ("emptyNoteF" .== EmptyNoteF)
              .+ ("valF"       .== ValF)
              .+ ("floatValF"  .== FloatValF)
              .+ ("addF"       .== AddF))

  -- type AST2 = OpenADT AST02
  type AST1 = OpenADT AST1F
  type AST0 = OpenADT AST0F


  -- interpret
  --------------------------------------------------------
