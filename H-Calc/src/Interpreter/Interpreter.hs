
module Interpreter.Interpreter where

  -- this module is the main entry point of the interpreter

  import Interpreter.A_Nucleus
  import Interpreter.B_Add
  import Interpreter.C_Mul
  import Interpreter.Transfos

  import Data.OpenADT
  import Fmt
  import Text.Megaparsec
  import Text.Show

  -- row-types
  import           Data.Row

  -- recursion-schemes
  import           Data.Functor.Foldable                    ( Fix(..), unfix, cata )

  -- main parser
  --------------------------------------------------------

  type AST = OpenADT (AST0F .+ ("mulF"       .== MulF))

  termParser :: MParser AST
  termParser
    = try floatValParser
    <|> valParser
    <|> do
          _ <- symbol "("
          e <- parser
          _ <- symbol ")"
          return e


  factorParser :: MParser AST
  factorParser
    = try (mulParser termParser)
    <|> termParser

  parser :: MParser AST
  parser
    = try (addParser factorParser)
    <|> factorParser


  -- evaluation
  --------------------------------------------------------

  data Result
    = RInt Int
    | RFloat Float
    | RError Text
    deriving (Show, Eq)

  eval :: AST0 -> Result
  eval l = caseonF r (unfix l) where
    r = #emptyNoteF   .== (\EmptyNoteF' -> RError "can't evaluate empty expression")
     .+ #hErrorF      .== (\(HErrorF' _ t) -> RError t)
     .+ #valF         .== (\(ValF' _ i) -> RInt i)
     .+ #floatValF    .== (\(FloatValF' _ f) -> RFloat f)
     .+ #addF         .== (\(AddF' _ (v1,v2)) ->
              case (eval v1, eval v2) of
                (RInt v1', RInt v2')     -> RInt (v1'+v2')
                (RFloat v1', RFloat v2') -> RFloat (v1'+v2')
                (RError e, _) -> RError e
                (_, RError e) -> RError e
                (a,b)         -> RError $ format "Error in eval({}+{})" (show a) (show b)
      )


  -- interpret
  --------------------------------------------------------

  interpret :: Text -> Result
  interpret source
    = case runParser parser "" source of
        Left _ -> RError "can't parse"
        Right a -> a
            & cata (Fix . diversifyF @("typF" .== TypF))
            & setType
            & distribute & demultiply
            & removeAnnotation
            & eval
