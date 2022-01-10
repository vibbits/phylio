module Test.Bio.Phylogeny.PhyloXml where

import Prelude
import Test.Bio.Phylogeny.Expect

import Bio.Phylogeny.PhyloXml (parsePhyloXml)
import Data.Tuple.Nested ((/\))
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Node.Path (sep)
import Test.Spec (Spec, describe, it)

specs :: Spec Unit
specs = do
  describe "Parse PhyloXML" do
    it "Parses an empty tree" do
      text <- readTextFile UTF8 $ "test" <> sep <> "data" <> sep <> "phyloxml_example1.xml"
      expectFail "Not a PhyloXML document" $ parsePhyloXml text

    it "Parses the PhyloXML example" do
      text <- readTextFile UTF8 $ "test" <> sep <> "data" <> sep <> "phyloxml_example2.xml"

      parsePhyloXml text
        `expectNames`
          [ ("" /\ 0.0), ("C" /\ 0.4), ("" /\ 0.06), ("B" /\ 0.23), ("A" /\ 0.102) ]

    it "Parses a simple example" do
      text <- readTextFile
        UTF8
        ("test" <> sep <> "data" <> sep <> "phyloxml_example_branch_length_attr.xml")
      parsePhyloXml text
        `expectNames`
          [ ("" /\ 0.0), ("D" /\ 0.4), ("A" /\ 0.06), ("C" /\ 0.23), ("B" /\ 0.102) ]

