module Bio.Phylogeny
  ( module Bio.Phylogeny.Internal.Attributes
  , Taxa
  , Phylogeny(Phylogeny)
  , attributes
  , dot
  , edges
  , lookupNode
  , roots
  , parseNewick
  , parseNexus
  , parsePhyloXml
  , reportError
  , vertices
  ) where

import Prelude

import Bio.Phylogeny.Internal.Attributes (Attribute(..))
import Bio.Phylogeny.Internal.Newick (parseNewick) as Internal
import Bio.Phylogeny.Internal.Nexus (parseNexus) as Internal
import Bio.Phylogeny.Internal.PhyloXml (parsePhyloXml) as Internal
import Bio.Phylogeny.Internal.Types
  ( NodeIdentifier
  , Phylogeny
  , PhylogenyNode(..)
  ) as Internal
import Data.Array ((!!))
import Data.Array as A
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Graph as G
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Utils (lines, repeat)
import Data.Traversable (intercalate)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Parsing (ParseError(..), Position(..))

type Taxa = Internal.PhylogenyNode

type Edge = Tuple Internal.NodeIdentifier Internal.NodeIdentifier

newtype Phylogeny = Phylogeny Internal.Phylogeny

-- Two graphs are equal if they have the same vertex set and the same set of edges.
instance eqPhylogeny :: Eq Phylogeny where
  eq a b =
    A.sort (lookupEdge a <$> edges a) == A.sort (lookupEdge b <$> edges b)
      && A.sort (vertices a) == A.sort (vertices b)

instance showPhylogeny :: Show Phylogeny where
  show (Phylogeny phylogeny) =
    "{Phylogenies: " <> (show (fromMaybe "#" <<< _.name <$> phylogeny.metadata)) <> "}"

graph :: Phylogeny -> G.Graph Internal.NodeIdentifier Internal.PhylogenyNode
graph (Phylogeny { network }) = network

roots :: Phylogeny -> Array Internal.NodeIdentifier
roots (Phylogeny { metadata }) = (_.parent) <$> metadata

edges :: Phylogeny -> Array Edge
edges phylogeny =
  foldl go [] (G.edges $ graph phylogeny)
  where
  go
    :: Array Edge
    -> { end :: Internal.NodeIdentifier, start :: Internal.NodeIdentifier }
    -> Array Edge
  go acc { end, start } = A.cons (start /\ end) acc

vertices :: Phylogeny -> Array Taxa
vertices phylogeny = A.fromFoldable $ G.vertices $ graph phylogeny

attributes :: Taxa -> Map String Attribute
attributes (Internal.PhylogenyNode node) =
  node.attributes

lookupNode :: Phylogeny -> Internal.NodeIdentifier -> Maybe Internal.PhylogenyNode
lookupNode (Phylogeny phylogeny) id =
  G.lookup id phylogeny.network

lookupEdge :: Phylogeny -> Edge -> Maybe (Tuple Taxa Taxa)
lookupEdge phylogeny (from /\ to) =
  case (lookupNode phylogeny from /\ lookupNode phylogeny to) of
    (Just a /\ Just b) -> Just (a /\ b)
    _ -> Nothing

-- Parsing
parseNewick :: String -> Either ParseError Phylogeny
parseNewick input = Phylogeny <$> Internal.parseNewick input

parsePhyloXml :: String -> Either ParseError Phylogeny
parsePhyloXml input = Phylogeny <$> Internal.parsePhyloXml input

parseNexus :: String -> Either ParseError Phylogeny
parseNexus input = Phylogeny <$> Internal.parseNexus input

reportError :: ParseError -> String -> String
reportError (ParseError msg (Position { line, column })) input =
  "ERROR: " <> msg <> "\n" <> offender <> "\n" <> location
  where
  offender = fromMaybe "???" $ lines input !! (line - 1)
  location = case repeat (column - 1) " " of
    Just padding -> padding <> "^"
    _ -> ""

dot :: Phylogeny -> String
dot phy =
  let
    extractPNode :: Taxa -> String
    extractPNode (Internal.PhylogenyNode node) =
      show node.ref <> " [label=\"" <> node.name <> "\"];"

    extractEdge :: Edge -> String
    extractEdge (from /\ to) =
      (show from) <> " -> " <> (show to)

  in
    "strict digraph {\n\n  "
      <> intercalate "\n  " (extractPNode <$> vertices phy)
      <> "\n\n  "
      <> intercalate "\n  " (extractEdge <$> edges phy)
      <> "\n}"
