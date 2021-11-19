module Test.Main where

import Bio.Phylogeny.Parser
import Bio.Phylogeny.Types
import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array as A
import Data.Either (Either(..))
import Data.Graph as G
import Data.List as L
import Data.Newtype (un)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (Error)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

graph :: Phylogeny -> G.Graph NodeIdentifier PNode
graph phy = un Network phy.network

nodeName :: PNode -> Tuple String Number
nodeName (PNode { name, branchLength }) = (name /\ branchLength)

nameNodes :: Phylogeny -> Array (Tuple String Number)
nameNodes phy =
  A.fromFoldable $ nodeName <$> (L.catMaybes $ flip G.lookup g <$> G.topologicalSort g)
  where
  g = graph phy

expectNNodes :: forall m. MonadThrow Error m => String -> Int -> m Unit
expectNNodes input n =
  (L.length <<< G.vertices <<< graph <$> parseNewick input)
    `shouldEqual`
      Right n

expectNames :: forall m. MonadThrow Error m => String -> Array (Tuple String Number) -> m Unit
expectNames input names =
  (nameNodes <$> parseNewick input)
    `shouldEqual`
      Right names

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Parse Newick" do
    it "Parses an empty sexpr" do
      expectNNodes "();" 2

    it "Parses a single node" do
      expectNames "A;" [ ("A" /\ 0.0) ]

    it "Parses a singleton sexpr" do
      expectNames "(A);" [ ("" /\ 0.0), ("A" /\ 0.0) ]

    it "Parses a string name" do
      expectNames "(Hello,World);" [ ("" /\ 0.0), ("World" /\ 0.0), ("Hello" /\ 0.0) ]

    it "Parses a sexpr with spaces" do
      expectNames "( A, B ,  C );" [ ("" /\ 0.0), ("C" /\ 0.0), ("B" /\ 0.0), ("A" /\ 0.0) ]

    it "Parses a single node with a branch length" do
      expectNames "A:1.1;" [ ("A" /\ 1.1) ]

    it "Parses a list with 3 unnamed nodes" do
      expectNames "(,,);" [ ("" /\ 0.0), ("" /\ 0.0), ("" /\ 0.0), ("" /\ 0.0) ]

    it "Parses a nested list with unnamed nodes" do
      expectNNodes "(,,(,));" 6

    it "Parses a nested list with named nodes" do
      expectNames "(A,B,(C,D));" [ ("" /\ 0.0), ("" /\ 0.0), ("D" /\ 0.0), ("C" /\ 0.0), ("B" /\ 0.0), ("A" /\ 0.0) ]

    it "Parses a rooted and nested tree with all named nodes" do
      expectNames "(A,(B)C)D;" [ ("D" /\ 0.0), ("C" /\ 0.0), ("B" /\ 0.0), ("A" /\ 0.0) ]

    it "Parses a rooted and nested tree with all named nodes and branch lengths" do
      expectNames "(A:1.0,(B:1.5)C:0.9)D:5.6;"
        [ ("D" /\ 5.6), ("C" /\ 0.9), ("B" /\ 1.5), ("A" /\ 1.0) ]

    it "Parses a rooted and nested tree with no named nodes but all branch lengths" do
      expectNames "(:1.0,(:1.5):0.9):5.6;"
        [ ("" /\ 5.6), ("" /\ 0.9), ("" /\ 1.5), ("" /\ 1.0) ]

  describe "Parse Newick wikipedia examples" do
    it "Parses no nodes are named" do
      expectNNodes "(,,(,));" 6

    it "Parses leaf nodes are named" do
      expectNames "(A,B,(C,D));"
        [ ("" /\ 0.0), ("" /\ 0.0), ("D" /\ 0.0), ("C" /\ 0.0), ("B" /\ 0.0), ("A" /\ 0.0) ]

    it "Parses all nodes are named" do
      expectNames "(A,B,(C,D)E)F;"
        [ ("F" /\ 0.0), ("E" /\ 0.0), ("D" /\ 0.0), ("C" /\ 0.0), ("B" /\ 0.0), ("A" /\ 0.0) ]

    it "Parses all but root node have a distance to parent" do
      expectNames "(:0.1,:0.2,(:0.3,:0.4):0.5);"
        [ ("" /\ 0.0), ("" /\ 0.5), ("" /\ 0.4), ("" /\ 0.3), ("" /\ 0.2), ("" /\ 0.1) ]

    it "Parses all have a distance to parent" do
      expectNames "(:0.1,:0.2,(:0.3,:0.4):0.5):0.0;"
        [ ("" /\ 0.0), ("" /\ 0.5), ("" /\ 0.4), ("" /\ 0.3), ("" /\ 0.2), ("" /\ 0.1) ]

    it "Parses distances and leaf names (popular)" do
      expectNames "(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);"
        [ ("" /\ 0.0), ("" /\ 0.5), ("D" /\ 0.4), ("C" /\ 0.3), ("B" /\ 0.2), ("A" /\ 0.1) ]

    it "Parses distances and all names" do
      expectNames "(A:0.1,B:0.2,(C:0.3,D:0.4)E:0.5)F;"
        [ ("F" /\ 0.0), ("E" /\ 0.5), ("D" /\ 0.4), ("C" /\ 0.3), ("B" /\ 0.2), ("A" /\ 0.1) ]

    it "Parses a tree rooted on a leaf node (rare)" do
      expectNames "((B:0.2,(C:0.3,D:0.4)E:0.5)F:0.1)A;"
        [ ("A" /\ 0.0), ("F" /\ 0.1), ("E" /\ 0.5), ("D" /\ 0.4), ("C" /\ 0.3), ("B" /\ 0.2) ]

  describe "Parse Extended Newick networks" do
    it "Parses an Extended Newick network" do
      expectNames "(A,B,((C,(Y)x#1)c,(x#1,D)d)e)f;"
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
