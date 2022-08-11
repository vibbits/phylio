module Test.Bio.Phylogeny.PhyloXml where

import Prelude

import Bio.Phylogeny.Types (Attribute(..))
import Bio.Phylogeny (attributes, parsePhyloXml, reportError, roots, vertices)
import Data.Array as A
import Data.Either (Either(..))
import Data.Graph as G
import Data.Map (lookup)
import Data.Maybe (maybe)
import Data.Tuple.Nested ((/\))
import Debug (traceM)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Test.Bio.Phylogeny.Expect (expectFail, expectNames, nodeName)
import Test.Path ((</>), testDir)
import Test.Spec (Spec, describe, it, itOnly)
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

    it "Parses a clade name in the shape NUMBER_LETTERS" do
      text <- readTextFile UTF8 $ testDir </> "NUMBER_LETTERS.phyloxml"

      parsePhyloXml text
        `expectNames`
          [ ("25_BRAFL" /\ 0.4534) ]

    itOnly "Parses a numeric attribute as XML tag attribute" do
      text <- readTextFile UTF8 $ testDir </> "attrs.xml"

      _ <- traceM text

      case parsePhyloXml text of
        Right tree -> do
          _ <- traceM $ show <<< Mapping <<< attributes <$> vertices tree
          fail "Parsed"
        Left err -> fail $ reportError err text

-- {
--   "empty": {"empty": {}},
--   "first": {
--       "first": {
--           "second": {
--               "third": {
--                   "fourth": 1.0
--               }
--           }
--       }
--   },
--   "numericAttr": [
--       {
--         "numericAttr": {"num": 123.0}
--       },
--       {
--         "numericAttr": {"num": 123.0}
--       }
--   ],
--   "textAttr": [
--       {
--           "textAttr": {
--               "textA": test 1,
--               "textB": test 2
--           }
--       },
--       {
--           "textAttr": [
--               {"textA": test},
--               {"textB": another}
--           ]
--       }
--   ]
-- }

-- Mapping: {
--   "empty": Mapping: {},
--   "first": Mapping: {
--     "fourth": Numeric: 1.0
---  },
--   "numericAttr": List: [
--       Mapping: {
--         "num": Numeric: 123.0
--       },
--       Mapping: {
--         "num": Numeric: 123.0
--       }
--   ],
--   "textAttr": List: [
--       Mapping: {
--         "textA": Text: test 1,
--         "textB": Text: test 2
--       },
--       List: [
--           Mapping: {"textA": Text: test},
--           Mapping: {"textB": Text: another}
--       ]
--   ]
-- }

-- Mapping: {
--   "empty": Mapping: {},
--   "first": Mapping: {
--     "second": Mapping: {
--       "third": Mapping: {
--         "fourth": Mapping: {
--           "fourth": Numeric: 1.0
--         }
--       }
--     }
--   },
--   "numericAttr": List: [
--       Mapping: {
--         "num": Numeric: 123.0
--       },
--       Mapping: {
--         "num": Mapping: {
--           "num": Numeric: 123.0
--         }
--       }
--   ],
--   "textAttr": List: [
--       Mapping: {
--         "textA": Text: test 1,
--         "textB": Text: test 2
--       },
--       Mapping: {
--         "textA": Mapping: {
--           "textA": Text: test
--         },
--         "textB": Mapping: {
--           "textB": Text: another
--         }
--       }
--   ]
-- }

-- Mapping: {
--   "empty": Mapping: {},
--   "first": Mapping: {
--     "second": Mapping: {
--       "third": Mapping: {
--         "fourth": Numeric: 1.0
--       }
--     }
--   },
--   "numericAttr": List: [
--       Mapping: {
--         "num": Numeric: 123.0
--       },
--       Mapping: {
--         "num": Numeric: 123.0
--       }
--   ],
--   "textAttr": List: [
--       Mapping: {
--         "textA": Text: test 1,
--         "textB": Text: test 2
--       },
--       Mapping: {
--         "textA": Text: test,
--         "textB": Text: another
--       }
--   ]
-- }

-- Mapping: {
--   "empty": Mapping: {},
--   "first": Mapping: {
--     "second": Mapping: {
--       "third": Mapping: {
--         "fourth": Numeric: 1.0
--       },
--       "x": Numeric: 5.0
--     }
--   },
--   "numericAttr": List: [Mapping: {"num": Numeric: 123.0},Mapping: {"num": Numeric: 123.0}], "textAttr": List: [Mapping: {"textA": Text: test 1, "textB": Text: test 2},Mapping: {"textA": Text: test, "textB": Text: another}]}
