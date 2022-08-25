module Test.Bio.Phylogeny.Nexus where

import Prelude

import Bio.Phylogeny (parseNexus)
import Data.Tuple.Nested ((/\))
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Node.Path (sep)
import Test.Bio.Phylogeny.Expect (expectFail, expectNames)
import Test.Spec (Spec, describe, it)

specs :: Spec Unit
specs = do
  describe "Parse Nexus" do
    it "Parses just a header" do
      expectFail "No valid phylogenies" $ parseNexus "#NEXUS"

    it "Parses 2 empty non-tree blocks" do
      expectFail "No valid phylogenies" $ parseNexus
        "#NEXUS\nBegin test;\n\nEND;\nBEGIN test;\n\nEND;"

    it "Parses wikipedia example" do
      text <- readTextFile UTF8 $ "test" <> sep <> "data" <> sep <> "wiki.nex"
      parseNexus text `expectNames`
        [ ("" /\ 0.0)
        , ("" /\ 0.0)
        , ("SpaceElf" /\ 0.0)
        , ("SpaceOrc" /\ 0.0)
        , ("" /\ 0.0)
        , ("SpaceCat" /\ 0.0)
        , ("SpaceDog" /\ 0.0)
        ]
