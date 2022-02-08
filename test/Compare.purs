module Test.Compare where

import Prelude

import Bio.Phylogeny (parsePhyloXml, parseNewick)
import Data.Map as M
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Test.Bio.Phylogeny.Expect (nameNodes)
import Test.Path ((</>), testDir)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

specs :: Spec Unit
specs = do
  describe "Compare PhyloXML with Newick Wikipedia examples" do
    it "Parses leaf nodes are named" do
      phyloxmlText <- readTextFile UTF8 $ testDir </> "leaf-nodes-named.xml"

      (M.fromFoldable <<< nameNodes <$> parseNewick "(A,B,(C,D));")
        `shouldEqual`
          (M.fromFoldable <<< nameNodes <$> parsePhyloXml phyloxmlText)

    it "Parses all nodes are named" do
      phyloxmlText <- readTextFile UTF8 $ testDir </> "all-nodes-named.xml"

      (M.fromFoldable <<< nameNodes <$> parseNewick "(A,B,(C,D)E)F;")
        `shouldEqual`
          (M.fromFoldable <<< nameNodes <$> parsePhyloXml phyloxmlText)

    it "Parses all but root node have a distance to parent" do
      phyloxmlText <- readTextFile UTF8 $ testDir </> "distances-and-leaf-names.xml"

      (M.fromFoldable <<< nameNodes <$> parseNewick "(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);")
        `shouldEqual`
          (M.fromFoldable <<< nameNodes <$> parsePhyloXml phyloxmlText)

  describe "Compare equivalent serialisations in different formats" do
    it "Newick and PhyloXML deserialise to the same network" do
      newickText <- readTextFile UTF8 $ testDir </> "Phaeobacter.newick"
      phyloxmlText <- readTextFile UTF8 $ testDir </> "Phaeobacter.phyloxml"

      (M.fromFoldable <<< nameNodes <$> parseNewick newickText)
        `shouldEqual`
          (M.fromFoldable <<< nameNodes <$> parsePhyloXml phyloxmlText)

