module Test.Compare where

import Prelude

import Bio.Phylogeny (parseNewick, parsePhyloXml)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Test.Path (testDir, (</>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

specs :: Spec Unit
specs = do
  describe "Compare PhyloXML with Newick Wikipedia examples" do
    it "Parses leaf nodes are named" do
      phyloxmlText <- readTextFile UTF8 $ testDir </> "leaf-nodes-named.xml"

      (parseNewick "(A,B,(C,D));")
        `shouldEqual`
          (parsePhyloXml phyloxmlText)

    it "Parses all nodes are named" do
      phyloxmlText <- readTextFile UTF8 $ testDir </> "all-nodes-named.xml"

      (parseNewick "(A,B,(C,D)E)F;")
        `shouldEqual`
          (parsePhyloXml phyloxmlText)

    it "Parses all but root node have a distance to parent" do
      phyloxmlText <- readTextFile UTF8 $ testDir </> "distances-and-leaf-names.xml"

      (parseNewick "(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);")
        `shouldEqual`
          (parsePhyloXml phyloxmlText)

  describe "Compare equivalent serialisations in different formats" do
    it "Phaeobacter Newick/PhyloXML" do
      newickText <- readTextFile UTF8 $ testDir </> "Phaeobacter.newick"
      phyloxmlText <- readTextFile UTF8 $ testDir </> "Phaeobacter.phyloxml"

      (parseNewick newickText)
        `shouldEqual`
          (parsePhyloXml phyloxmlText)

    it "Dicots 1 Newick/PhyloXML" do
      newickText <- readTextFile UTF8 $ testDir </> "dicots1.newick"
      phyloxmlText <- readTextFile UTF8 $ testDir </> "dicots1.xml"

      (parseNewick newickText)
        `shouldEqual`
          (parsePhyloXml phyloxmlText)

    it "Bacillus Newick/PhyloXML" do
      newickText <- readTextFile UTF8 $ testDir </> "bacillus.newick"
      phyloxmlText <- readTextFile UTF8 $ testDir </> "bacillus.xml"

      (parseNewick newickText)
        `shouldEqual`
          (parsePhyloXml phyloxmlText)

    it "Pangolin SARS-CoV-2 Newick/PhyloXML" do
      newickText <- readTextFile UTF8 $ testDir </> "pangolin.newick"
      phyloxmlText <- readTextFile UTF8 $ testDir </> "pangolin.xml"

      (parseNewick newickText)
        `shouldEqual`
          (parsePhyloXml phyloxmlText)
