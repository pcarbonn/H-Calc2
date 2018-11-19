
module Interpreter.A_Nucleus where

  -- this module defines the following nodes
  --    (HError Text)
  --    (EmptyNote)
  --    (Typ t α)
  -------------------------------------------------------

  import Interpreter.Transfos

  import Data.OpenADT
  import Fmt
  -- import Text.Megaparsec
  import Text.Megaparsec.Char as M
  import qualified Text.Megaparsec.Char.Lexer as L
  import Text.Show


  -- row-types
  import           Data.Row

-- recursion-schemes
  import           Data.Functor.Foldable                    ( unfix )


  -- parser
  --------------------------------------------------------
  spaceConsumer :: MParser ()
  spaceConsumer = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

  symbol :: Text -> MParser Text
  symbol = L.symbol spaceConsumer



  -- AST nodes
  -------------------------------------------------------

  data TType = TInt | TFloat deriving (Show, Eq)

  data EmptyNoteF e = EmptyNoteF'     deriving (Eq, Functor, Show)
  data HErrorF    e = HErrorF' e Text deriving (Eq, Functor, Show)
  data TypF       e = TypF' e TType   deriving (Eq, Functor, Show)

  mkVarPattern ''EmptyNoteF "emptyNoteF" "EmptyNote" "EmptyNoteF"
  mkVarPattern ''HErrorF    "hErrorF"    "HError"    "HErrorF"
  mkVarPattern ''TypF       "typF"       "Typ"       "TypF"

  type NucleusRowF   = (("emptyNoteF" .== EmptyNoteF)
                     .+ ("hErrorF"    .== HErrorF))
                     .+ ("typF"       .== TypF)

  type NucleusRow = OpenADT NucleusRowF

  -- Transformations
  --------------------------------------------------------

  -- algebra

  instance Algebra EmptyNoteF where
    showAST' EmptyNoteF' = ""

  instance Algebra HErrorF where
    showAST' (HErrorF' α s) = format "{}{}" s α

  instance Algebra TypF where
    showAST' (TypF' α t) = format " :: {}{}" (show t) α

  -- isomorphism

  instance ( OpenAlg xs "emptyNoteF" EmptyNoteF (OpenADT xs))
           => Isomorphism xs HErrorF where
    getAnnotation (HErrorF' α _) = α
    setType' _ = EmptyNote

  instance ( OpenAlg xs "typF" TypF (OpenADT xs))
          => Isomorphism xs TypF where
    getAnnotation (TypF' α t) = Typ α t
    setType' (TypF' α _) = α -- erase existing type

  instance ( OpenAlg xs "emptyNoteF" EmptyNoteF (OpenADT xs))
        => Isomorphism xs EmptyNoteF where
    getAnnotation EmptyNoteF' = EmptyNote
    setType' _ = EmptyNote


  -- other

  getType :: ( OpenAlg xs "typF" TypF (OpenADT xs)
             , OpenAlg xs "emptyNoteF" EmptyNoteF (OpenADT xs)
             , Forall xs (Isomorphism xs)
             ) => OpenADT xs -> Maybe TType
  getType = go . getAnnotation . unfix
    where go (Typ _ t) = Just t
          go EmptyNote = Nothing -- no annotation anymore
          go α = getType $ getAnnotation $ unfix α


  -- Tree reduction : EADT xs -> EADT ys
  -------------------------------------------------------

  instance (OpenAlg xs "emptyNoteF" EmptyNoteF (OpenADT xs)) => RemoveAnnotation xs TypF where
    removeAnnotation' (TypF' _ _) = EmptyNote

  instance (OpenAlg xs "emptyNoteF" EmptyNoteF (OpenADT xs)) => RemoveAnnotation xs EmptyNoteF where
    removeAnnotation' (EmptyNoteF') = EmptyNote

  instance ( OpenAlg xs "emptyNoteF" EmptyNoteF (OpenADT xs)
           , OpenAlg xs "hErrorF" HErrorF (OpenADT xs))
           => RemoveAnnotation xs HErrorF where
    removeAnnotation' (HErrorF' _ e) = HError EmptyNote e
