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
      parse (scan "x") `shouldBe` [Eval (Info (Position 1 1)) (Var (Info (Position 1 1)) "x")]
    it "parses a lambda" $ do
      parse (scan "lambda x.x") `shouldBe` [Eval (Info (Position 1 1)) (Abs (Info (Position 1 1)) "x" (Var (Info (Position 1 10)) "x"))] 
    it "deals with parens and application" $ do
      parse (scan "(lambda x. x) (lambda y. y)") `shouldBe` [Eval (Info (Position 1 2)) (App (Info (Position 1 2)) (Abs (Info (Position 1 2)) "x" (Var (Info (Position 1 12)) "x")) (Abs (Info (Position 1 16)) "y" (Var (Info (Position 1 26)) "y")))]
    it "deals with empty input" $ do
      parse (scan "") `shouldBe` []
        
