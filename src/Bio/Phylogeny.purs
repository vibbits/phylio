module Bio.Phylogeny
  ( module Bio.Phylogeny.Internal.Attributes
  , Taxa
  , Phylogeny(Phylogeny)
  , Metadata
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
  , meta
  ) where

import Prelude

import Bio.Phylogeny.Internal.Attributes (Attribute(..))
import Bio.Phylogeny.Internal.Newick (parseNewick) as Internal
import Bio.Phylogeny.Internal.Nexus (parseNexus) as Internal
import Bio.Phylogeny.Internal.PhyloXml (parsePhyloXml) as Internal
import Bio.Phylogeny.Internal.Types
  ( Metadata
  , NodeIdentifier
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

-- | A node in the phylogeny graph.
type Taxa = Internal.PhylogenyNode

-- | Metatdata about a specific phylogeny
type Metadata = Internal.Metadata

-- | An edge between nodes in the phylogeny graph.
-- | The first value is the source and the second is the sink.
-- | The `ref` for each `Taxa` is used. You can use `lookupNode` to get
-- | a `Taxa` from a `ref`.
type Edge = Tuple Internal.NodeIdentifier Internal.NodeIdentifier

-- | A Phylogeny graph with assocuated metadata.
-- | Multiple trees may be stored by their roots.
newtype Phylogeny = Phylogeny Internal.Phylogeny

-- Two graphs are equal if they have the same vertex set and the same set of edges.
instance eqPhylogeny :: Eq Phylogeny where
  eq a b =
    A.sort (lookupEdge a <$> edges a) == A.sort (lookupEdge b <$> edges b)
      && A.sort (vertices a) == A.sort (vertices b)

instance showPhylogeny :: Show Phylogeny where
  show (Phylogeny phylogeny) =
    "{Phylogenies: " <> (show (fromMaybe "#" <<< _.name <$> phylogeny.metadata)) <> "}"

-- | Given a `Phylogeny` get the `Graph` representation
graph :: Phylogeny -> G.Graph Internal.NodeIdentifier Internal.PhylogenyNode
graph (Phylogeny { network }) = network

-- | The metadata for each tree in this `Phylogeny`.
meta :: Phylogeny -> Array Metadata
meta (Phylogeny { metadata }) = metadata

-- | Get the root for each tree in a `Phylogeny`.
roots :: Phylogeny -> Array Internal.NodeIdentifier
roots (Phylogeny { metadata }) = (_.parent) <$> metadata

-- | Get _all_ edges in a `Phylogeny`.
-- | Note that this includes edges for disconnected trees.
edges :: Phylogeny -> Array Edge
edges phylogeny =
  foldl go [] (G.edges $ graph phylogeny)
  where
  go
    :: Array Edge
    -> { end :: Internal.NodeIdentifier, start :: Internal.NodeIdentifier }
    -> Array Edge
  go acc { end, start } = A.cons (start /\ end) acc

-- | Get _all_ vertices (nodes) in a `Phylogeny`.
-- | Note that this includes vertices from disconnected trees.
vertices :: Phylogeny -> Array Taxa
vertices phylogeny = A.fromFoldable $ G.vertices $ graph phylogeny

-- | Get the attribute map for a given `Taxa`.
attributes :: Taxa -> Map String Attribute
attributes (Internal.PhylogenyNode node) =
  node.attributes

-- | Given a `Phylogeny` and a `ref` (identifier) get the `Taxa` for that `ref`.
lookupNode :: Phylogeny -> Internal.NodeIdentifier -> Maybe Internal.PhylogenyNode
lookupNode (Phylogeny phylogeny) id =
  G.lookup id phylogeny.network

lookupEdge :: Phylogeny -> Edge -> Maybe (Tuple Taxa Taxa)
lookupEdge phylogeny (from /\ to) =
  case (lookupNode phylogeny from /\ lookupNode phylogeny to) of
    (Just a /\ Just b) -> Just (a /\ b)
    _ -> Nothing

-- Parsing

-- | Parse a phylogeny serialised with the Newick format.
-- | Some variations of Newick are also supported.
parseNewick :: String -> Either ParseError Phylogeny
parseNewick input = Phylogeny <$> Internal.parseNewick input

-- | Parse a phylogeny serialised with the Nexus format
parseNexus :: String -> Either ParseError Phylogeny
parseNexus input = Phylogeny <$> Internal.parseNexus input

-- | Parse a phylogeny serialised with the PhyloXML format
parsePhyloXml :: String -> Either ParseError Phylogeny
parsePhyloXml input = Phylogeny <$> Internal.parsePhyloXml input

-- | When a `ParseError` occurs, you can use this function to turn
-- | it into a helpful error message. The second argument is the
-- | input that failed parsing.
-- |
-- | ```purescript run
-- | > lmap (parseNewick ")") (reportError ")")
-- | Left ")\n^"
-- | ```
reportError :: String -> ParseError -> String
reportError input (ParseError msg (Position { line, column })) =
  "ERROR: " <> msg <> "\n" <> offender <> "\n" <> location
  where
  offender = fromMaybe "???" $ lines input !! (line - 1)
  location = case repeat (column - 1) " " of
    Just padding -> padding <> "^"
    _ -> ""

-- | Generate a GraphViz DOT representation of this phylogeny
-- | For more information about DOT see this URL:
-- | https://graphviz.org/doc/info/lang.html
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
