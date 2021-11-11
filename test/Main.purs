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
  describe "Parse Newick" do
    it "Parses an empty sexpr" do
      parseNewick "();" `shouldEqual` Right (Internal ("" /\ 0.0) [Leaf ("" /\ 0.0)])

    it "Parses a single node" do
      parseNewick "A;" `shouldEqual` Right (Leaf ("A" /\ 0.0))

    it "Parses a singleton sexpr" do
      parseNewick "(A);" `shouldEqual` Right (Internal ("" /\ 0.0) [Leaf ("A" /\ 0.0)])

    it "Parses a string name" do
      parseNewick "(Hello,World);" `shouldEqual` Right (Internal ("" /\ 0.0) [Leaf ("Hello" /\ 0.0), Leaf ("World" /\ 0.0)])

    it "Parses a sexpr with spaces" do
      parseNewick "( A, B ,  C );" `shouldEqual` Right (Internal ("" /\ 0.0) [Leaf ("A" /\ 0.0), Leaf ("B" /\ 0.0), Leaf ("C" /\ 0.0)])

    it "Parses a single node with a branch length" do
      parseNewick "A:1.0;" `shouldEqual` Right (Leaf ("A" /\ 1.0))

    it "Parses a list with 3 unnamed nodes" do
      parseNewick "(,,);" `shouldEqual` Right (Internal ("" /\ 0.0) [Leaf ("" /\ 0.0), Leaf ("" /\ 0.0), Leaf ("" /\ 0.0)])

    it "Parses a nested list with unnamed nodes" do
      parseNewick "(,,(,));" `shouldEqual` Right (Internal ("" /\ 0.0) [Leaf ("" /\ 0.0), Leaf ("" /\ 0.0), Internal ("" /\ 0.0) [Leaf ("" /\ 0.0), Leaf ("" /\ 0.0)]])

    it "Parses a nested list with named nodes" do
      parseNewick "(A,B,(C,D));" `shouldEqual` Right (Internal ("" /\ 0.0) [Leaf ("A" /\ 0.0), Leaf ("B" /\ 0.0), Internal ("" /\ 0.0) [Leaf ("C" /\ 0.0), Leaf ("D" /\ 0.0)]])

    it "Parses a rooted and nested tree with all named nodes" do
      parseNewick "(A,(B)C)D;" `shouldEqual` Right (Internal ("D" /\ 0.0) [Leaf ("A" /\ 0.0), Internal ("C" /\ 0.0) [Leaf ("B" /\ 0.0)]])

    it "Parses a rooted and nested tree with all named nodes and branch lengths" do
      parseNewick "(A:1.0,(B:1.5)C:0.9)D:5.6;"
        `shouldEqual`
        Right (Internal ("D" /\ 5.6) [Leaf ("A" /\ 1.0), Internal ("C" /\ 0.9) [Leaf ("B" /\ 1.5)]])

    it "Parses a rooted and nested tree with all no named nodes but all branch lengths" do
      parseNewick "(:1.0,(:1.5):0.9):5.6;"
        `shouldEqual`
        Right (Internal ("" /\ 5.6) [Leaf ("" /\ 1.0), Internal ("" /\ 0.9) [Leaf ("" /\ 1.5)]])

  describe "Parse Newick wikipedia examples" do
    it "Parses no nodes are named" do
      parseNewick "(,,(,));"
        `shouldEqual`
        Right (Internal ("" /\ 0.0) [Leaf ("" /\ 0.0), Leaf ("" /\ 0.0), Internal ("" /\ 0.0) [Leaf ("" /\ 0.0), Leaf ("" /\ 0.0)]])

    it "Parses leaf nodes are named" do
      parseNewick "(A,B,(C,D));"
        `shouldEqual`
        Right (Internal ("" /\ 0.0) [Leaf ("A" /\ 0.0), Leaf ("B" /\ 0.0), Internal ("" /\ 0.0) [Leaf ("C" /\ 0.0), Leaf ("D" /\ 0.0)]])

    it "Parses all nodes are named" do
      parseNewick "(A,B,(C,D)E)F;"
        `shouldEqual`
        Right (Internal ("F" /\ 0.0) [Leaf ("A" /\ 0.0), Leaf ("B" /\ 0.0), Internal ("E" /\ 0.0) [Leaf ("C" /\ 0.0), Leaf ("D" /\ 0.0)]])

    it "Parses all but root node have a distance to parent" do
      parseNewick "(:0.1,:0.2,(:0.3,:0.4):0.5);"
        `shouldEqual`
        Right (Internal ("" /\ 0.0) [Leaf ("" /\ 0.1), Leaf ("" /\ 0.2), Internal ("" /\ 0.5) [Leaf ("" /\ 0.3), Leaf ("" /\ 0.4)]])

    it "Parses all have a distance to parent" do
      parseNewick "(:0.1,:0.2,(:0.3,:0.4):0.5):0.0;"
        `shouldEqual`
        Right (Internal ("" /\ 0.0) [Leaf ("" /\ 0.1), Leaf ("" /\ 0.2), Internal ("" /\ 0.5) [Leaf ("" /\ 0.3), Leaf ("" /\ 0.4)]])

    it "Parses distances and leaf names (popular)" do
      parseNewick "(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);"
        `shouldEqual`
        Right (Internal ("" /\ 0.0) [Leaf ("A" /\ 0.1), Leaf ("B" /\ 0.2), Internal ("" /\ 0.5) [Leaf ("C" /\ 0.3), Leaf ("D" /\ 0.4)]])

    it "Parses distances and all names" do
      parseNewick "(A:0.1,B:0.2,(C:0.3,D:0.4)E:0.5)F;"
        `shouldEqual`
        Right (Internal ("F" /\ 0.0) [Leaf ("A" /\ 0.1), Leaf ("B" /\ 0.2), Internal ("E" /\ 0.5) [Leaf ("C" /\ 0.3), Leaf ("D" /\ 0.4)]])

    it "Parses a tree rooted on a leaf node (rare)" do
      parseNewick "((B:0.2,(C:0.3,D:0.4)E:0.5)F:0.1)A;"
        `shouldEqual`
        Right (Internal ("A" /\ 0.0) [Internal ("F" /\ 0.1) [Leaf ("B" /\ 0.2), Internal ("E" /\ 0.5) [Leaf ("C" /\ 0.3), Leaf ("D" /\ 0.4)]]])
