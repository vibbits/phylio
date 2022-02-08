module Test.Bio.Phylogeny.PhyloXml where

import Prelude

import Bio.Phylogeny (parsePhyloXml, reportError, roots)
import Data.Array as A
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Test.Bio.Phylogeny.Expect (expectNames, expectFail, nodeName)
import Test.Path ((</>), testDir)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

specs :: Spec Unit
specs = do
  describe "Parse PhyloXML" do
    it "Parses an empty tree" do
      text <- readTextFile UTF8 $ testDir </> "phyloxml_example1.xml"
      expectFail "No trees in this PhyloXML document" $ parsePhyloXml text

    it "Parses the PhyloXML example" do
      text <- readTextFile UTF8 $ testDir </> "phyloxml_example2.xml"

      let phylogeny = parsePhyloXml text

      (A.length <<< roots <$> phylogeny)
        `shouldEqual`
          (Right 13)

      case phylogeny of
        Right phylogeny' ->
          (nodeName <$> roots phylogeny')
            `shouldEqual`
              [ ("root 1" /\ 0.0)
              , ("root 2" /\ 0.0)
              , ("root 3" /\ 0.0)
              , ("root 4" /\ 0.0)
              , ("root 5" /\ 0.0)
              , ("root 6" /\ 0.0)
              , ("root 7" /\ 0.0)
              , ("root 8" /\ 0.0)
              , ("root 9" /\ 0.0)
              , ("root 10" /\ 0.0)
              , ("root 11" /\ 0.0)
              , ("root 12" /\ 0.0)
              , ("root 13" /\ 0.0)
              ]
        Left err -> fail $ reportError err ""

    it "Parses branch lengths in clade attributes" do
      text <- readTextFile UTF8 $ testDir </> "phyloxml_example_branch_length_attr.xml"

      parsePhyloXml text
        `expectNames`
          [ ("" /\ 0.0), ("D" /\ 0.4), ("A" /\ 0.06), ("C" /\ 0.23), ("B" /\ 0.102) ]

    it "Parses branch lengths in nodes" do
      text <- readTextFile UTF8 $ testDir </> "phyloxml_example_branch_length_node.xml"

      parsePhyloXml text
        `expectNames`
          [ ("" /\ 0.0), ("D" /\ 0.4), ("A" /\ 0.06), ("C" /\ 0.23), ("B" /\ 0.102) ]

