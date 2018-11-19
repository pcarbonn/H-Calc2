
module Interpreter.Transfos where

  import Prelude
  import           Data.OpenADT

  -- row-types
  import           Data.Row

  -- recursion-schemes
  import           Data.Functor.Foldable                    (cata --, Fix(..)
                                                            )


  -- This modules declares the AST-wide transformations
  -------------------------------------------------------


  -- Algebra :: EADT xs -> Fixed type
  -------------------------------------------------------
  class Algebra (f :: * -> *) where
    showAST' :: f Text -> Text
    -- add more algebra here

  instance (Forall v Algebra) => Algebra (VarF v) where
    showAST' = varFAlg @Algebra showAST'

  showAST :: ( Forall xs Functor, Forall xs Algebra)
    => OpenADT xs -> Text
  showAST = cata showAST'


  -- Isomorphism :: EADT xs -> EADT xs
  -------------------------------------------------------

  class Isomorphism xs (f :: * -> *) where
    getAnnotation :: f (OpenADT xs) -> OpenADT xs
    setType'      :: f (OpenADT xs) -> OpenADT xs
    -- add more isomorphisms here

  instance ( Forall xs (Isomorphism ys))
          => Isomorphism ys (VarF xs) where
    getAnnotation = varFAlg @(Isomorphism ys) getAnnotation
    setType'      = varFAlg @(Isomorphism ys) setType'

  setType :: ( Forall xs Functor, Forall xs (Isomorphism xs) )
             => OpenADT xs -> OpenADT xs
  setType = cata setType'

  -- Tree Expansion : EADT xs -> EADT ys
  -------------------------------------------------------
  -- --> appendEADT @'[newConstructor], followed by isomorphism



  -- Tree reduction : EADT xs -> EADT ys
  -------------------------------------------------------

  -- removeAnnotation

  class RemoveAnnotation ys (f :: * -> *) where
    removeAnnotation'      :: f (OpenADT ys) -> OpenADT ys

  instance (Forall xs (RemoveAnnotation ys)) => RemoveAnnotation ys (VarF xs) where
    removeAnnotation' = varFAlg @(RemoveAnnotation ys) removeAnnotation'

  -- instance {-# OVERLAPPABLE #-} RemoveAnnotation ys f where
  --   removeAnnotation' = Fix . VarF -- VF -- if f is in result type, keep as is

  removeAnnotation :: (Forall xs Functor, RemoveAnnotation ys (VarF xs))
      => OpenADT xs -> OpenADT ys
  removeAnnotation = cata removeAnnotation'
