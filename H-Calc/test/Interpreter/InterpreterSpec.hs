{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Interpreter.InterpreterSpec (spec) where

  import Interpreter.Interpreter

  import Test.Hspec
  import Relude hiding (fromInteger, fromRational)

  shouldBe' :: Text -> Text -> Expectation
  shouldBe' = shouldBe

  spec :: Spec
  spec = do
    describe "Interpreter" $ do
      it "interprets" $ do
        show (interpret "-5") `shouldBe'` "RInt (-5)"
      it "interprets" $ do
        show (interpret "-5.3") `shouldBe'` "RFloat (-5.3)"
      it "interprets" $ do
        show (interpret "((2 +1 {- test -})*5.0)") `shouldBe'` "RFloat 15.0"
