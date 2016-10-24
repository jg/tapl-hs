module MainSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Alexer
import HParser
import Syntax

spec :: Spec
spec = do
  describe "Lexer" $ do
    it "lexes single lambda" $ do
      test "lambda x. x" `shouldBe`
        [Lambda,Id "x",Dot,Id "x"]
    it "lexes two lambdas" $ do
      test "lambda y. lambda x. x y" `shouldBe`
        [Lambda,Id "y",Dot,Lambda,Id "x",Dot,Id "x",Id "y"]
    it "lexes lambdas in parens" $ do
      test "(lambda x. x) (lambda x. x x)" `shouldBe`
        [Sym '(',Lambda,Id "x",Dot,Id "x",Sym ')',Sym '(',Lambda,Id "x",Dot,Id "x",Id "x",Sym ')']

  describe "Parser" $ do
    it "parses a variable eval" $ do
      parse (test "x") `shouldBe` [Eval (Var "x")]
    it "parses a lambda" $ do
      parse (test "lambda x.x") `shouldBe` [Eval (Abs "x" (Var "x"))]
    it "deals with parens and application" $ do
      parse (test "(lambda x. x) (lambda y. y)") `shouldBe` [Eval (App (Abs "x" (Var "x")) (Abs "y" (Var "y")))]
    it "deals with empty input" $ do
      parse (test "") `shouldBe` []
        
