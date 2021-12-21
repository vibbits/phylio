module Test.Bio.Phylogeny.PhyloXml where

import Debug
import Prelude
import Test.Bio.Phylogeny.Expect

import Bio.Phylogeny.PhyloXml (XmlNode(..), parsePhyloXml)
import Bio.Phylogeny.Types (Tree(..), Attribute(..))
import Data.Either (Either(..))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Node.Path (sep)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

specs :: Spec Unit
specs = do
  describe "Parse PhyloXML" do
    it "Parses an empty tree" do
      text <- readTextFile UTF8 $ "test" <> sep <> "data" <> sep <> "phyloxml_example1.xml"
      parsePhyloXml text
        `shouldEqual`
          Right
            ( Leaf
                ( XmlNode
                    { name: "phyloxml"
                    , attributes:
                        ( M.fromFoldable
                            [ ( "xsi:schemaLocation"
                                  /\ Text "http://www.phyloxml.org http://www.phyloxml.org/1.10/phyloxml.xsd"
                              )
                            ]
                        )
                    , value: Nothing
                    }
                )
            )

    --     it "Parses an indented region" do
    --       let
    --         text =
    --           """<clade branch_length="0.102">
    --     <name>A</name>
    -- </clade>"""
    --       let _ = trace (parsePhyloXml text) identity
    --       fail "HERE"

    it "Parses an empty tag" do
      let
        text =
          """<sequence_relation id_ref_0="x" id_ref_1="y" type="paralogy"/>"""

      let _ = trace (parsePhyloXml text) identity
      fail "HERE2"

    it "Parses the PhyloXML example" do
      text <- readTextFile UTF8 $ "test" <> sep <> "data" <> sep <> "phyloxml_example2.xml"
      let _ = trace (parsePhyloXml text) identity
      fail "Expected fail"
