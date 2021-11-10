module Test.Main where

import Prelude

import Bio.Phylogeny.Parser (Phylogeny(..), parseNewick)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Parse sexpr" do
    it "Parses an empty sexpr" do
      parseNewick "();" `shouldEqual` Right (Internal [Leaf ("" /\ 0.0)])

    it "Parses a single node" do
      parseNewick "A;" `shouldEqual` Right (Leaf ("A" /\ 0.0))

    it "Parses a singleton sexpr" do
      parseNewick "(A);" `shouldEqual` Right (Internal [Leaf ("A" /\ 0.0)])

    it "Parses a string name" do
      parseNewick "(Hello,World);" `shouldEqual` Right (Internal [Leaf ("Hello" /\ 0.0), Leaf ("World" /\ 0.0)])

    it "Parses a sexpr with spaces" do
      parseNewick "( A, B ,  C );" `shouldEqual` Right (Internal [Leaf ("A" /\ 0.0), Leaf ("B" /\ 0.0), Leaf ("C" /\ 0.0)])

    it "Parses a single node with a branch length" do
      parseNewick "A:1.0;" `shouldEqual` Right (Leaf ("A" /\ 1.0))

    it "Parses a list with 3 unnamed nodes" do
      parseNewick "(,,);" `shouldEqual` Right (Internal [Leaf ("" /\ 0.0), Leaf ("" /\ 0.0), Leaf ("" /\ 0.0)])

    it "Parses a nested list with unnamed nodes" do
      parseNewick "(,,(,));" `shouldEqual` Right (Internal [Leaf ("" /\ 0.0), Leaf ("" /\ 0.0), Internal [Leaf ("" /\ 0.0), Leaf ("" /\ 0.0)]])

    it "Parses a nested list with named nodes" do
      parseNewick "(A,B,(C,D));" `shouldEqual` Right (Internal [Leaf ("A" /\ 0.0), Leaf ("B" /\ 0.0), Internal [Leaf ("C" /\ 0.0), Leaf ("D" /\ 0.0)]])

    it "Parses a rooted tree with all named nodes" do
      parseNewick "(A,(B)C)D;" `shouldEqual` Right (Internal [Leaf ("A" /\ 0.0), Internal [Leaf ("B" /\ 0.0)], Leaf ("C" /\ 0.0)])
