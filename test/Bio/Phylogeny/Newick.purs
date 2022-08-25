module Test.Bio.Phylogeny.Newick where

import Prelude

import Bio.Phylogeny (parseNewick)
import Data.Either (isLeft)
import Data.Tuple.Nested ((/\))
import Test.Bio.Phylogeny.Expect (expectNNodes, expectNames)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldSatisfy)

specs :: Spec Unit
specs = do
  describe "Parse Newick"
    do
      it "Parses an empty phylogeny" do
        parseNewick "();" `expectNNodes` 2

      it "Parses an single semicolon phylogeny" do
        parseNewick ";" `expectNNodes` 1

      it "Fails to parse phylogeny with missing semicolon" do
        parseNewick "()" `shouldSatisfy` isLeft

      it "Fails to parse phylogeny with missing closing paren" do
        parseNewick "(" `shouldSatisfy` isLeft

      it "Fails to parse phylogeny with missing opening paren" do
        parseNewick ");" `shouldSatisfy` isLeft

      it "Parses a single node" do
        parseNewick "A;" `expectNames` [ ("A" /\ 0.0) ]

      it "Parses a singleton phylogeny" do
        parseNewick "(A);"
          `expectNames`
            [ ("" /\ 0.0), ("A" /\ 0.0) ]

      it "Parses a string name" do
        parseNewick "(Hello,World);"
          `expectNames`
            [ ("" /\ 0.0), ("World" /\ 0.0), ("Hello" /\ 0.0) ]

      it "Parses a phylogeny with spaces" do
        parseNewick "( A, B ,  C );"
          `expectNames`
            [ ("" /\ 0.0), ("C" /\ 0.0), ("B" /\ 0.0), ("A" /\ 0.0) ]

      it "Parses a name that contains spaces" do
        parseNewick "(A node, B node);"
          `expectNames`
            [ ("" /\ 0.0), ("B node" /\ 0.0), ("A node" /\ 0.0) ]

      it "Parses a single node with a branch length" do
        parseNewick "A:1.1;" `expectNames` [ ("A" /\ 1.1) ]

      it "Parses a list with 3 unnamed nodes" do
        parseNewick "(,,);"
          `expectNames`
            [ ("" /\ 0.0), ("" /\ 0.0), ("" /\ 0.0), ("" /\ 0.0) ]

      it "Parses a nested list with unnamed nodes" do
        parseNewick "(,,(,));" `expectNNodes` 6

      it "Parses a nested list with named nodes" do
        parseNewick "(A,B,(C,D));"
          `expectNames`
            [ ("" /\ 0.0), ("" /\ 0.0), ("D" /\ 0.0), ("C" /\ 0.0), ("B" /\ 0.0), ("A" /\ 0.0) ]

      it "Parses a rooted and nested tree with all named nodes" do
        parseNewick "(A,(B)C)D;"
          `expectNames`
            [ ("D" /\ 0.0), ("C" /\ 0.0), ("B" /\ 0.0), ("A" /\ 0.0) ]

      it "Parses a rooted and nested tree with all named nodes and branch lengths" do
        parseNewick "(A:1.0,(B:1.5)C:0.9)D:5.6;"
          `expectNames`
            [ ("D" /\ 5.6), ("C" /\ 0.9), ("B" /\ 1.5), ("A" /\ 1.0) ]

      it "Parses a rooted and nested tree with no named nodes but all branch lengths" do
        parseNewick "(:1.0,(:1.5):0.9):5.6;"
          `expectNames`
            [ ("" /\ 5.6), ("" /\ 0.9), ("" /\ 1.5), ("" /\ 1.0) ]

      it "Parses branch lengths in scientific notation" do
        parseNewick "A:-1.5e-1;" `expectNames` [ ("A" /\ -0.15) ]

  describe "Parse Newick wikipedia examples"
    do
      it "Parses no nodes are named" do
        parseNewick "(,,(,));" `expectNNodes` 6

      it "Parses leaf nodes are named" do
        parseNewick "(A,B,(C,D));"
          `expectNames`
            [ ("" /\ 0.0), ("" /\ 0.0), ("D" /\ 0.0), ("C" /\ 0.0), ("B" /\ 0.0), ("A" /\ 0.0) ]

      it "Parses all nodes are named" do
        parseNewick "(A,B,(C,D)E)F;"
          `expectNames`
            [ ("F" /\ 0.0), ("E" /\ 0.0), ("D" /\ 0.0), ("C" /\ 0.0), ("B" /\ 0.0), ("A" /\ 0.0) ]

      it "Parses all but root node have a distance to parent" do
        parseNewick "(:0.1,:0.2,(:0.3,:0.4):0.5);"
          `expectNames`
            [ ("" /\ 0.0), ("" /\ 0.5), ("" /\ 0.4), ("" /\ 0.3), ("" /\ 0.2), ("" /\ 0.1) ]

      it "Parses all have a distance to parent" do
        parseNewick "(:0.1,:0.2,(:0.3,:0.4):0.5):0.0;"
          `expectNames`
            [ ("" /\ 0.0), ("" /\ 0.5), ("" /\ 0.4), ("" /\ 0.3), ("" /\ 0.2), ("" /\ 0.1) ]

      it "Parses distances and leaf names (popular)" do
        parseNewick "(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);"
          `expectNames`
            [ ("" /\ 0.0), ("" /\ 0.5), ("D" /\ 0.4), ("C" /\ 0.3), ("B" /\ 0.2), ("A" /\ 0.1) ]

      it "Parses distances and all names" do
        parseNewick "(A:0.1,B:0.2,(C:0.3,D:0.4)E:0.5)F;"
          `expectNames`
            [ ("F" /\ 0.0), ("E" /\ 0.5), ("D" /\ 0.4), ("C" /\ 0.3), ("B" /\ 0.2), ("A" /\ 0.1) ]

      it "Parses a tree rooted on a leaf node (rare)" do
        parseNewick "((B:0.2,(C:0.3,D:0.4)E:0.5)F:0.1)A;"
          `expectNames`
            [ ("A" /\ 0.0), ("F" /\ 0.1), ("E" /\ 0.5), ("D" /\ 0.4), ("C" /\ 0.3), ("B" /\ 0.2) ]

  describe "Parse ad-hoc extensions" do
    it "Parses tree with MrBayes comments" do
      parseNewick "(8[&prob=1.0]:2.94e-1[&length_mean=2.9e-1])[&prob=1.0][&length_mean=0];"
        `expectNames`
          [ ("" /\ 0.0)
          , ("8" /\ 0.294)
          ]

    it "Parses a tree with [] comments" do
      parseNewick "(Archaea:0.43325[100]):0.88776;"
        `expectNames`
          [ ("" /\ 0.88776)
          , ("Archaea" /\ 0.43325)
          ]

    it "Fails to parse a tree with non-closed comment" do
      parseNewick "(Archaea:0.43325[100):0.88776;"
        `shouldSatisfy` isLeft

  describe "Parse Extended Newick networks"
    do
      it "Parses an Extended Newick network" do
        parseNewick "(A,B,((C,(Y)x#1)c,(x#1,D)d)e)f;"
          `expectNames`
            [ ("f" /\ 0.0)
            , ("e" /\ 0.0)
            , ("d" /\ 0.0)
            , ("D" /\ 0.0)
            , ("c" /\ 0.0)
            , ("C" /\ 0.0)
            , ("B" /\ 0.0)
            , ("A" /\ 0.0)
            , ("x" /\ 0.0)
            , ("Y" /\ 0.0)
            ]

      it "Parses diamond network" do
        parseNewick "((d#1)b,(d#1)c)a;"
          `expectNames`
            [ ("a" /\ 0.0)
            , ("c" /\ 0.0)
            , ("b" /\ 0.0)
            , ("d" /\ 0.0)
            ]

  describe "Parse NHX"
    do
      it "Parses empty NHX attributes" do
        parseNewick "A[&&NHX:];" `expectNNodes` 1

      it "Parses a single NHX node" do
        parseNewick "ADH2:0.1[&&NHX:S=human:E=1.1.1.1];" `expectNNodes` 1

      it "Parses an NHX sample" do
        parseNewick
          "(((ADH2:0.1[&&NHX:S=human:E=1.1.1.1], ADH1:0.11[&&NHX:S=human:E=1.1.1.1]):0.05[&&NHX:S=Primates:E=1.1.1.1:D=Y:B=100], ADHY:0.1[&&NHX:S=nematode:E=1.1.1.1],ADHX:0.12[&&NHX:S=insect:E=1.1.1.1]):0.1[&&NHX:S=Metazoa:E=1.1.1.1:D=N], (ADH4:0.09[&&NHX:S=yeast:E=1.1.1.1],ADH3:0.13[&&NHX:S=yeast:E=1.1.1.1], ADH2:0.12[&&NHX:S=yeast:E=1.1.1.1],ADH1:0.11[&&NHX:S=yeast:E=1.1.1.1]):0.1 [&&NHX:S=Fungi])[&&NHX:E=1.1.1.1:D=N];"
          `expectNNodes`
            12

