module Interpreter.C_Mul where

  -- this module adds the following language construct to the DSL
  --    (Mul α (i1,i2))
  -------------------------------------------------------

  import Interpreter.A_Nucleus
  import Interpreter.B_Add
  import Interpreter.Transfos

  import Data.OpenADT
  import Fmt
  import Text.Show

  -- recursion-schemes
  import           Data.Functor.Foldable                    ( Fix(..), unfix )

  -- row-types
  import           Data.Row

  -- define nodes
  --------------------------------------------------------

  data MulF      e =      MulF' e (e, e) deriving (Eq, Functor, Show)
  mkVarPattern ''MulF      "mulF"      "Mul"      "MulF"

  (.*) ::         ( OpenAlg xs "emptyNoteF" EmptyNoteF (OpenADT xs)
                  , OpenAlg xs "mulF" MulF (OpenADT xs))
                => OpenADT xs -> OpenADT xs -> OpenADT xs
  (.*) a b = Mul EmptyNote (a,b)


  type AST2F = AST0F .+ ("mulF"       .== MulF) .+ ("typF"       .== TypF)
  type AST2 = OpenADT AST2F


  -- parser
  --------------------------------------------------------

  mulParser ::    ( OpenAlg xs "emptyNoteF" EmptyNoteF (OpenADT xs)
                  , OpenAlg xs "mulF" MulF (OpenADT xs))
      => MParser (OpenADT xs) -> MParser (OpenADT xs)
  mulParser factorP = Mul EmptyNote <$> do
    i1 <- factorP
    _ <- symbol "*"
    i2 <- factorP
    return (i1,i2)


  -- Algebra
  --------------------------------------------------------

  instance Algebra MulF where
    showAST' (MulF' α (v1,v2)) = format "({} * {}){}" v1 v2 α


  -- Isomorphism
  --------------------------------------------------------

  instance  ( OpenAlg xs "emptyNoteF" EmptyNoteF (OpenADT xs)
            , OpenAlg xs "hErrorF" HErrorF (OpenADT xs)
            , OpenAlg xs "typF" TypF (OpenADT xs)
            , OpenAlg xs "mulF" MulF (OpenADT xs)
            , Forall xs (Isomorphism xs)
            , Forall xs Functor, Forall xs Algebra )
          => Isomorphism xs MulF where
    getAnnotation (MulF' α _) = α
    setType' (MulF' α (i, v)) =
      case (i, v) of
        (HError _ _, _) -> i
        (_, HError _ _) -> v
        _ -> case (getType i, getType v) of
                (Just TInt, Just TInt)   -> Mul (Typ α TInt  ) (i,v)
                (Just TInt, Just TFloat) -> Mul (Typ α TFloat) (i,v)
                (Just TFloat, Just TInt) -> Mul (Typ α TFloat) (i,v)
                (Just t1  , Just t2)     ->
                         HError α $ format "can't multiply `{}` whose type is {} with `{}` whose type is "
                                    (showAST i) (show t1) (showAST v) (show t2)
                (_,_) -> HError α "Missing type info in multiplication"


  -- apply distribution : a*(b+c) -> (a*b+a*c)
  --------------------------------------------------------

  distribute :: AST2 -> AST2
  distribute x = case trialF (unfix x) #mulF of
      Right other -> Fix $ diversifyF @("mulF" .== MulF) $ fmap d other
      Left (MulF' α (v1,v2)) -> go α (v1,v2)
      where
        d = distribute
        go α (i, (Add β (v1,v2))) = Add β (d (Mul α (i,d v1)), d (Mul α (i,d v2)))
        go α ((Add β (v1,v2)), i) = Add β (d (Mul α (d v1,i)), d (Mul α (d v2,i)))
        go α (v1,v2)              = Mul α (d v1, d v2)


  -- demultiply : n*a -> a+a+... n times
  --------------------------------------------------------

  demultiply :: AST2 -> AST1
  demultiply x = case trialF (unfix x) #mulF of
    Right other -> Fix $ fmap d other
    Left (MulF' α (v1,v2)) ->
      case (d v1, d v2) of
        (HError _ e, _) -> HError (d α) e
        (_, HError _ e) -> HError (d α) e
        (Val _ i1, v2') ->
                if  | i1 < 0 -> HError (d α) $
                                format "Error: can't multiply by negative number {}" i1
                    | i1 == 0 -> Val (d α) 0
                    | i1 == 1 -> v2'
                    | otherwise -> Add (d α) (d v2, d $ Mul α ((Val α $ i1-1), v2))
        (_, Val _ _) -> d (Mul α (v2,v1))
        (_, _) -> HError (d α) $ format "Can't multiply by {}" (showAST v1)
    where
      d = demultiply
