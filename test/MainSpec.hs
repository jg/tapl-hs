module MainSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Alexer

spec :: Spec
spec = do
  describe "Lexer" $ do
    it "lexes" $ do
      test "lambda x. x" `shouldBe`
        [Lambda,Var "x",Dot,Var "x"]
    it "lexes" $ do
      test "lambda y. lambda x. x y" `shouldBe`
        [Lambda,Var "y",Dot,Lambda,Var "x",Dot,Var "x",Var "y"]
      test "(lambda x. x) (lambda x. x x)" `shouldBe`
        [Sym '(',Lambda,Var "x",Dot,Var "x",Sym ')',Sym '(',Lambda,Var "x",Dot,Var "x",Var "x",Sym ')']
