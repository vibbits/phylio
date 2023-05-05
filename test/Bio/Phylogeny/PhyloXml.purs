module Test.Bio.Phylogeny.PhyloXml where

import Prelude

import Bio.Phylogeny
  ( Attribute(..)
  , attributes
  , edges
  , lookupNode
  , parsePhyloXml
  , reportError
  , roots
  , vertices
  )
import Data.Array as A
import Data.Either (Either(..))
import Data.Map (empty, fromFoldable)
import Data.Tuple.Nested ((/\))
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Test.Bio.Phylogeny.Expect (expectFail, expectNames, nodeName)
import Test.Path (testDir, (</>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

specs :: Spec Unit
specs = do
  describe "Parse PhyloXML" do
    it "Parses an empty tree" do
      text <- readTextFile UTF8 $ testDir </> "phyloxml_example1.xml"
      expectFail "No trees in this PhyloXML document" $ parsePhyloXml text

    it "Parses a single node tree" do
      let text = "<phyloxml><phylogeny><clade><name>A</name></clade></phylogeny></phyloxml>"
      let phylogeny = parsePhyloXml text

      (A.length <<< roots <$> phylogeny)
        `shouldEqual`
          (Right 1)

      case phylogeny of
        Right phylogeny' ->
          (nodeName <$> (A.catMaybes (lookupNode phylogeny' <$> (roots phylogeny'))))
            `shouldEqual`
              [ ("A" /\ 0.0) ]
        Left err -> fail $ reportError "" err

    it "Parses a single node tree with name attribute" do
      let text = "<phyloxml><phylogeny><clade name=\"A\" /></phylogeny></phyloxml>"
      let phylogeny = parsePhyloXml text

      (A.length <<< roots <$> phylogeny)
        `shouldEqual`
          (Right 1)

      case phylogeny of
        Right phylogeny' ->
          (nodeName <$> (A.catMaybes (lookupNode phylogeny' <$> (roots phylogeny'))))
            `shouldEqual`
              [ ("A" /\ 0.0) ]
        Left err -> fail $ reportError "" err

    it "Parses a 4 node tree with a fork" do
      let
        text =
          "<phyloxml><phylogeny rooted=\'true\'><clade name=\"A\"><clade><name>B</name></clade></clade></phylogeny></phyloxml>"
      let phylogeny = parsePhyloXml text

      (A.length <<< roots <$> phylogeny)
        `shouldEqual`
          (Right 1)

      case phylogeny of
        Right phylogeny' -> do
          (nodeName <$> vertices phylogeny')
            `shouldEqual`
              [ ("A" /\ 0.0)
              , ("B" /\ 0.0)
              ]
          edges phylogeny'
            `shouldEqual`
              [ (1 /\ 2) ]
        Left err -> fail $ reportError "" err

    it "Parses the PhyloXML example" do
      text <- readTextFile UTF8 $ testDir </> "phyloxml_example2.xml"

      let phylogeny = parsePhyloXml text

      (A.length <<< roots <$> phylogeny)
        `shouldEqual`
          (Right 13)

      case phylogeny of
        Right phylogeny' ->
          (nodeName <$> (A.catMaybes (lookupNode phylogeny' <$> (roots phylogeny'))))
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
        Left err -> fail $ reportError "" err

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

    it "Parses a clade name in the shape NUMBER_LETTERS" do
      text <- readTextFile UTF8 $ testDir </> "NUMBER_LETTERS.phyloxml"

      parsePhyloXml text
        `expectNames`
          [ ("25_BRAFL" /\ 0.4534) ]

    it "Parses attributes" do
      text <- readTextFile UTF8 $ testDir </> "attrs.xml"

      case parsePhyloXml text of
        Right tree -> do
          (attributes <$> vertices tree)
            `shouldEqual`
              [ fromFoldable
                  [ ("empty" /\ Mapping empty)
                  , ( "first" /\ Mapping
                        ( fromFoldable
                            [ ( "second" /\ Mapping
                                  ( fromFoldable
                                      [ ("x" /\ Numeric 5.0)
                                      , ( "third" /\ Mapping
                                            ( fromFoldable
                                                [ ("fourth" /\ Numeric 1.0) ]
                                            )
                                        )
                                      ]
                                  )
                              )
                            ]
                        )
                    )
                  , ( "numericAttr" /\ List
                        [ Mapping (fromFoldable [ ("num" /\ Numeric 123.0) ])
                        , Mapping (fromFoldable [ ("num" /\ Numeric 123.0) ])
                        ]
                    )
                  , ( "textAttr" /\ List
                        [ Mapping
                            ( fromFoldable
                                [ ("textA" /\ Text "test 1")
                                , ("textB" /\ Text "test 2")
                                ]
                            )
                        , Mapping
                            ( fromFoldable
                                [ ("textA" /\ Text "test")
                                , ("textB" /\ Text "another")
                                ]
                            )
                        ]
                    )
                  , ( "property" /\ Mapping
                        ( fromFoldable
                            [ ("applies_to" /\ Text "clade")
                            , ("datatype" /\ Text "xsd:anyURI")
                            , ("ref" /\ Text "See also")
                            , ("value" /\ Text "https://vib.be")
                            ]
                        )
                    )
                  ]
              ]

        Left err -> fail $ reportError text err

