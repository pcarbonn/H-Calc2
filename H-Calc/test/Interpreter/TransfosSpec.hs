{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Interpreter.TransfosSpec (spec) where

  import Interpreter.A_Nucleus
  import Interpreter.Transfos

  import Test.Hspec
  import Relude hiding (fromInteger, fromRational)

  spec :: Spec
  spec = do
    describe "Utils" $ do
      it "shows EmptyNote" $ do
        showAST (EmptyNote :: NucleusRow) `shouldBe` ""
      it "says ok" $ do
        "ok" `shouldBe` ("ok" :: Text)
