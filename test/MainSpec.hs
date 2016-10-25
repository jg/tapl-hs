module MainSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Alexer
import HParser
import Syntax
import Control.Exception

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
    -- it "parses a variable eval" $ do
    --   evaluate (parse (scan "x")) `shouldThrow` errorCall "Identifier x is unbound"
    it "parses a lambda" $ do
      parse (scan "lambda x.x") `shouldBe` [Eval (Info (Position 1 1)) (Abs (Info (Position 1 1)) "x" (Var (Info (Position 1 10)) "x" 0 1))] 
    it "deals with parens and application" $ do
      parse (scan "(lambda x. x) (lambda y. y)") `shouldBe` [Eval (Info (Position 1 2)) (App (Info (Position 1 2)) (Abs (Info (Position 1 2)) "x" (Var (Info (Position 1 12)) "x" 0 1)) (Abs (Info (Position 1 16)) "y" (Var (Info (Position 1 26)) "y" 0 1)))]
    it "deals with nexted lambdas" $ do
      parse (scan "(lambda x. x (lambda y. x y))") `shouldBe` [Eval (Info (Position 1 2)) (Abs (Info (Position 1 2)) "x" (App (Info (Position 1 12)) (Var (Info (Position 1 12)) "x" 0 1) (Abs (Info (Position 1 15)) "y" (App (Info (Position 1 25)) (Var (Info (Position 1 25)) "x" 1 2) (Var (Info (Position 1 27)) "y" 0 2)))))]
    it "deals with empty input" $ do
      parse (scan "") `shouldBe` []
        
