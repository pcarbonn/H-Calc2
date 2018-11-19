module Interpreter.B_Add where

  -- this module adds the following language construct to the DSL
  --    (Val α i)
  --    (Add α (i1,i2))
  -------------------------------------------------------

  import Interpreter.A_Nucleus
  import Interpreter.Transfos

  import Data.OpenADT
  import Fmt
  import Text.Megaparsec
  import Text.Megaparsec.Char as M
  import Text.Show

  -- row-types
  import           Data.Row

  -- define nodes
  --------------------------------------------------------

  data ValF      e =      ValF' e Int    deriving (Eq, Functor, Show)
  data FloatValF e = FloatValF' e Float  deriving (Eq, Functor, Show)
  data AddF      e =      AddF' e (e, e) deriving (Eq, Functor, Show)

  mkVarPattern ''ValF      "valF"      "Val"      "ValF"
  mkVarPattern ''FloatValF "floatValF" "FloatVal" "FloatValF"
  mkVarPattern ''AddF      "addF"      "Add"      "AddF"

  type AST1F = AST0F .+ ("typF"       .== TypF)
  type AST0F  = (("hErrorF"    .== HErrorF)
              .+ ("emptyNoteF" .== EmptyNoteF)
              .+ ("valF"       .== ValF)
              .+ ("floatValF"  .== FloatValF)
              .+ ("addF"       .== AddF))

  type AST1 = OpenADT AST1F
  type AST0 = OpenADT AST0F

  -- syntactic sugar for embedded DSL
  --------------------------------------------------------

  fromInteger ::  ( OpenAlg xs "emptyNoteF" EmptyNoteF (OpenADT xs)
                  , OpenAlg xs "valF" ValF (OpenADT xs))
                => Integer -> OpenADT xs
  fromInteger i = Val EmptyNote $ fromIntegral i

  fromRational :: ( OpenAlg xs "emptyNoteF" EmptyNoteF (OpenADT xs)
                  , OpenAlg xs "floatValF" FloatValF (OpenADT xs))
                => Rational -> OpenADT xs
  fromRational i = FloatVal EmptyNote $ realToFrac i

  (.+.) ::         ( OpenAlg xs "emptyNoteF" EmptyNoteF (OpenADT xs)
                  , OpenAlg xs "addF" AddF (OpenADT xs))
                => OpenADT xs -> OpenADT xs -> OpenADT xs
  (.+.) a b = Add EmptyNote (a,b)

  neg ::          ( OpenAlg xs "emptyNoteF" EmptyNoteF (OpenADT xs)
                  , OpenAlg xs "hErrorF" HErrorF (OpenADT xs)
                  , OpenAlg xs "valF" ValF (OpenADT xs)
                  , Forall xs (Isomorphism xs)
                  , Forall xs Functor, Forall xs Algebra )
                => OpenADT xs -> OpenADT xs
  neg (Val      α i) = Val      α (-i)
  neg v = HError EmptyNote $ format "can't negate {}" (showAST v)



  -- parser
  --------------------------------------------------------

  valParser :: ( OpenAlg xs "emptyNoteF" EmptyNoteF (OpenADT xs)
                  , OpenAlg xs "valF" ValF (OpenADT xs))
        => MParser (OpenADT xs)
  valParser = Val EmptyNote . toInt <$> do
        s <- option "+" (string "-")
        i <- some M.digitChar
        _ <- option () spaceConsumer
        return (s,i)
    where toInt :: (Text, [Char]) -> Int
          toInt (s, cs) = s' * (foldl' (\a i -> a * 10 + digitToInt i) 0  cs)
            where s' = if s == "+" then 1 else -1

          digitToInt :: Char -> Int
          digitToInt c = ord c - ord '0'

  floatValParser :: ( OpenAlg xs "emptyNoteF" EmptyNoteF (OpenADT xs)
                  , OpenAlg xs "floatValF" FloatValF (OpenADT xs))
        => MParser (OpenADT xs)
  floatValParser = FloatVal EmptyNote . toFloat <$> do
          s <- option "+" (string "-")
          i1 <- some M.digitChar
          _ <- string "."
          i2 <- some M.digitChar
          _ <- option () spaceConsumer
          return (s, i1, i2)
    where toFloat :: (Text, [Char], [Char]) -> Float
          toFloat (s, i1,i2)
            = s' * (foldl' (\a i -> a * 10.0 + realToFrac (digitToInt i)) 0.0 i1
                   + (foldl' (\a i -> a * 10.0 + realToFrac (digitToInt i)) 0.0 i2)
                   / (10.0 ^ (length i2))
                  )
              where s' = if s == "+" then 1 else -1

          digitToInt :: Char -> Int
          digitToInt c = ord c - ord '0'

  addParser ::    ( OpenAlg xs "emptyNoteF" EmptyNoteF (OpenADT xs)
                  , OpenAlg xs "addF" AddF (OpenADT xs))
      => MParser (OpenADT xs) -> MParser (OpenADT xs)
  addParser termP = Add EmptyNote <$> do
    i1 <- termP
    _ <- symbol "+"
    i2 <- termP
    return (i1,i2)

  -- Algebra
  --------------------------------------------------------

  instance Algebra ValF where
    showAST' (ValF' α i) = format "{}{}" i α

  instance Algebra FloatValF where
      showAST' (FloatValF' α f) = format "{}{}" f α

  instance Algebra AddF where
    showAST' (AddF' α (v1,v2)) = format "({} + {}){}" v1 v2 α -- no recursive call



  -- Isomorphism
  --------------------------------------------------------

  instance ( OpenAlg xs "typF" TypF (OpenADT xs)
           , OpenAlg xs "valF" ValF (OpenADT xs))
           => Isomorphism xs ValF where
    getAnnotation (ValF' α _) = α
    setType' (ValF' α i) = Val (Typ α TInt) i

  instance ( OpenAlg xs "typF" TypF (OpenADT xs)
           , OpenAlg xs "floatValF" FloatValF (OpenADT xs))
           => Isomorphism xs FloatValF where
    getAnnotation (FloatValF' α _) = α
    setType' (FloatValF' α f) = FloatVal (Typ α TFloat) f

  instance  ( OpenAlg xs "emptyNoteF" EmptyNoteF (OpenADT xs)
            , OpenAlg xs "hErrorF" HErrorF (OpenADT xs)
            , OpenAlg xs "typF" TypF (OpenADT xs)
            , OpenAlg xs "addF" AddF (OpenADT xs)
            , Forall xs (Isomorphism xs)
            , Forall xs Functor, Forall xs Algebra )
          => Isomorphism xs AddF where
    getAnnotation (AddF' α _) = α
    setType' (AddF' α (v1, v2)) =
      case (v1,v2) of
        (HError _ _, _) -> v1
        (_, HError _ _) -> v2
        _ -> case (getType v1, getType v2) of
                (Just TInt  , Just TInt  ) -> Add (Typ α TInt) (v1,v2)
                (Just TFloat, Just TFloat) -> Add (Typ α TFloat) (v1,v2)
                (Just t1    , Just t2    ) ->
                         HError α $ format "can't add `{}` whose type is {} with `{}` whose type is "
                                    (showAST v1) (show t1) (showAST v2) (show t2)
                (_,_) -> HError α "Missing type info in addition"



  -- Tree reduction : EADT xs -> EADT ys
  -------------------------------------------------------


  instance ( OpenAlg xs "addF" AddF (OpenADT xs)
           , OpenAlg xs "emptyNoteF" EmptyNoteF (OpenADT xs))
           => RemoveAnnotation xs AddF where
    removeAnnotation' (AddF' _ (v1, v2)) = Add EmptyNote (v1, v2)

  instance ( OpenAlg xs "valF" ValF (OpenADT xs)
           , OpenAlg xs "emptyNoteF" EmptyNoteF (OpenADT xs))
           => RemoveAnnotation xs ValF where
    removeAnnotation' (ValF' _ i) = Val EmptyNote i

  instance ( OpenAlg xs "floatValF" FloatValF (OpenADT xs)
           , OpenAlg xs "emptyNoteF" EmptyNoteF (OpenADT xs))
           => RemoveAnnotation xs FloatValF where
    removeAnnotation' (FloatValF' _ f) = FloatVal EmptyNote f
