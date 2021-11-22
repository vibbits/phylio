module Bio.Phylogeny.PhyloXml where

import Prelude hiding (between)

import Bio.Phylogeny.Types (NodeType(..), PNode(..), Parser, Phylogeny, Tree(..), interpretIntermediate)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.Combinators (between)
import Text.Parsing.Parser.String (string)

parsePhyloXml :: String -> Either String Phylogeny
parsePhyloXml input = lmap show $ runParser input phyloXmlParser

phyloXmlParser :: Parser Phylogeny
phyloXmlParser = interpretIntermediate <$> xmltag "phyloxml" (pure $ Leaf pnode)
  where
  pnode :: PNode
  pnode = PNode
    { name: "test"
    , node: Clade
    , branchLength: 0.0
    , ref: Nothing
    , attributes: M.empty
    }

xmlopen :: String -> Parser Unit
xmlopen tag = (string $ "<" <> tag <> ">") *> pure unit

xmlclose :: String -> Parser Unit
xmlclose tag = (string $ "</" <> tag <> ">") *> pure unit

xmltag :: forall a. String -> Parser a -> Parser a
xmltag tag inner = between (xmlopen tag) (xmlclose tag) inner
