module Test.Main where

import Prelude

import Bio.Phylogeny.Parser (parseNewick)
import Data.Either (Either(..))
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
      parseNewick "();" `shouldEqual` Right []

    it "Parses a single node" do
      parseNewick "A;" `shouldEqual` Right ["A"]

    it "Parses a singleton sexpr" do
      parseNewick "(A);" `shouldEqual` Right ["A"]

    it "Parses a string name" do
      parseNewick "(Hello,World);" `shouldEqual` Right ["Hello", "World"]

    it "Parses a sexpr with spaces" do
      parseNewick "( A, B ,  C );" `shouldEqual` Right ["A", "B", "C"]
