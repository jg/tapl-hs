module MainSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Alexer
import HParser
import Syntax
import Control.Exception

epos = Info (Position 0 0) -- empty position
bvar = Var epos
v x = bvar "" x 0
lambda = Abs epos
l = lambda ""
app = App epos
ap = app

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

  describe "Parser (positions)" $ do
    -- it "parses a variable eval" $ do
    --   evaluate (parse (scan "x")) `shouldThrow` errorCall "Identifier x is unbound"
    it "parses a lambda" $ do
      parse (scan "lambda x.x") `shouldBe` [Eval (Info (Position 1 1)) (Abs (Info (Position 1 1)) "x" (Var (Info (Position 1 10)) "x" 0 1))] 
    it "deals with parens and application" $ do
      parse (scan "(lambda x. x) (lambda x. x x)") `shouldBe` [Eval (Info (Position 1 2)) (App (Info (Position 1 2)) (Abs (Info (Position 1 2)) "x" (Var (Info (Position 1 12)) "x" 0 1)) (Abs (Info (Position 1 16)) "x" (App (Info (Position 1 26)) (Var (Info (Position 1 26)) "x" 0 1) (Var (Info (Position 1 28)) "x" 0 1))))]
    it "deals with nexted lambdas" $ do
      parse (scan "(lambda x. x (lambda y. x y))") `shouldBe` [Eval (Info (Position 1 2)) (Abs (Info (Position 1 2)) "x" (App (Info (Position 1 12)) (Var (Info (Position 1 12)) "x" 0 1) (Abs (Info (Position 1 15)) "y" (App (Info (Position 1 25)) (Var (Info (Position 1 25)) "x" 1 2) (Var (Info (Position 1 27)) "y" 0 2)))))]
    it "deals with empty input" $ do
      parse (scan "") `shouldBe` []


  describe "termShift" $ do
    it "shifts correctly (1)" $ do
      let
        -- ^2 (L. L. 1 0 2)
        t = lambda "y" (lambda "x" (app (bvar "y" 1 2) (app (bvar "x" 0 2) (bvar "z" 2 2))))
        in brujin (termShift 2 t) `shouldBe` "L. L. 1 0 4"
    it "shifts correctly (2)" $ do
      let
        -- ^2 L. 0 1 L. 0 1 2
        t = l (ap (v 0) (app (v 1) (l (app (v 0) (ap (v 1) (v 2))))))
        in brujin (termShift 2 t) `shouldBe` "L. 0 3 L. 0 1 4"

  describe "termSubst" $ do
    -- [j |-> s] t
    it "[0 |-> 1 L. 2] (0 L. 1)" $ do
      let
        j = 0
        s = ap (v 1) (l (v 2))
        t = ap (v 0) (l (v 1))
        in brujin (termSubst j s t) `shouldBe` "1 L. 2 L. 2 L. 3"
    it "[0 |-> 1] L. 1 0" $ do
      let
        j = 0
        s = v 1
        t = l (ap (v 1) (v 0))
        in brujin (termSubst j s t) `shouldBe` "L. 2 0"
    it "[0 |-> L. 1 2] L. 1" $ do
      let
        j = 0
        s = l (ap (v 1) (v 2))
        t = l (v 1)
        in brujin (termSubst j s t) `shouldBe` "L. L. 2 3"
    it "[0 |-> 1] 0 L. L. 2" $ do
      let
        j = 0
        s = v 1
        t = ap (v 0) (l (l (v 2)))
        in brujin (termSubst j s t) `shouldBe` "1 L. L. 3"
        

      

      
