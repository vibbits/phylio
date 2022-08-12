module Bio.Phylogeny
  ( Taxa
  , Phylogeny(Phylogeny)
  , attrsToForeign
  , attributes
  , dot
  , edges
  , roots
  , parseNewick
  , parseNexus
  , parsePhyloXml
  , reportError
  , traverseNetwork
  , nodeTypeToString
  , vertices
  ) where

import Prelude

import Bio.Phylogeny.Newick (parseNewick) as Internal
import Bio.Phylogeny.Nexus (parseNexus) as Internal
import Bio.Phylogeny.PhyloXml (parsePhyloXml) as Internal
import Bio.Phylogeny.Types
  ( Attribute(..)
  , Network(..)
  , NodeIdentifier
  , NodeType
  , PNode(..)
  , PNodeInternal
  , Phylogeny
  , getRef
  , nodeTypeToString
  ) as Internal
import Data.Array ((!!))
import Data.Array as A
import Data.Either (Either)
import Data.Filterable (filterMap)
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Graph as G
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.String.Utils (lines, repeat)
import Data.Traversable (intercalate)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Foreign (Foreign)
import Parsing (Position(..), ParseError(..))

newtype Phylogeny = Phylogeny Internal.Phylogeny

-- Two graphs are equal if they have the same vertex set and the same set of edges.
instance eqPhylogeny :: Eq Phylogeny where
  eq a b =
    A.sort (edges a) == A.sort (edges b)
      && A.sort (vertices a) == A.sort (vertices b)
      && A.sort (roots a) == A.sort (roots b)

instance showPhylogeny :: Show Phylogeny where
  show (Phylogeny p) = show p

type Taxa = Internal.PNode

nodeTypeToString :: Internal.NodeType -> String
nodeTypeToString = Internal.nodeTypeToString

attrsToForeign
  :: { text :: (String -> String -> Foreign)
     , numeric :: (String -> Number -> Foreign)
     , bool :: (String -> Boolean -> Foreign)
     , list :: (String -> Array Internal.Attribute -> Foreign)
     , mapping :: (String -> Map String Internal.Attribute -> Foreign)
     }
  -> (Foreign -> Foreign -> Foreign)
  -> Foreign
  -> Map String Internal.Attribute
  -> Foreign
attrsToForeign { text, numeric, bool, list, mapping } f init attrs =
  foldrWithIndex gen init attrs
  where
  gen :: String -> Internal.Attribute -> Foreign -> Foreign
  gen k v acc =
    case v of
      Internal.Numeric n -> f acc $ numeric k n
      Internal.Text t -> f acc $ text k t
      Internal.Bool b -> f acc $ bool k b
      Internal.List as -> f acc $ list k as
      Internal.Mapping m -> f acc $ mapping k m

graph :: Phylogeny -> G.Graph Internal.NodeIdentifier Internal.PNode
graph (Phylogeny { network }) = un Internal.Network network

roots' :: Phylogeny -> Array Int
roots' (Phylogeny { metadata }) = (_.parent) <$> metadata

roots :: Phylogeny -> Array Taxa
roots phylogeny = A.catMaybes $ (flip G.lookup graph') <$> roots' phylogeny
  where
  graph' = graph phylogeny

edges :: Phylogeny -> Array (Tuple Taxa Taxa)
edges phylogeny =
  foldl go [] $ G.edges g
  where
  g :: G.Graph Internal.NodeIdentifier Internal.PNode
  g = graph phylogeny

  go :: Array (Tuple Taxa Taxa) -> G.Edge Internal.NodeIdentifier -> Array (Tuple Taxa Taxa)
  go acc { end, start } =
    case (G.lookup start g /\ G.lookup end g) of
      (Just source /\ Just sink) -> A.snoc acc (source /\ sink)
      _ -> acc

vertices :: Phylogeny -> Array Taxa
vertices phylogeny = A.fromFoldable $ G.vertices $ graph phylogeny

attributes :: Taxa -> Map String Internal.Attribute
attributes (Internal.PNode node) =
  node.attributes

traverseNetwork
  :: (Internal.PNodeInternal -> Array Int -> Internal.PNodeInternal)
  -> Phylogeny
  -> Phylogeny
traverseNetwork f (Phylogeny { metadata, network }) =
  Phylogeny
    { metadata: metadata
    , network: (Internal.Network $ map nodeF theGraph)
    }
  where
  theGraph :: G.Graph Int Taxa
  theGraph = un Internal.Network network

  nodeF :: Taxa -> Taxa
  nodeF pnode@(Internal.PNode node) =
    case node.ref of
      Just ref' -> Internal.PNode
        $ f node
        $ fromMaybe []
        $ A.fromFoldable <$> G.outEdges ref' theGraph
      _ -> pnode

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
  offender = case lines input !! (line - 1) of
    Just l -> l
    Nothing -> "????"
  location = case repeat (column - 1) " " of
    Just padding -> padding <> "^"
    _ -> ""

dot :: Phylogeny -> String
dot phy =
  let
    extractPNode :: Taxa -> String
    extractPNode tax@(Internal.PNode node) =
      case Internal.getRef tax of
        Just ref' -> show ref' <> " [label=\"" <> node.name <> "\"];"
        Nothing -> "_" <> " [label=\"" <> node.name <> "\"];"

    extractEdge :: Tuple Taxa Taxa -> Maybe String
    extractEdge (from /\ to) =
      case ((Internal.getRef from) /\ (Internal.getRef to)) of
        ((Just from') /\ (Just to')) -> Just (show from' <> " -> " <> show to')
        _ -> Nothing
  in
    "strict digraph {\n\n  "
      <> intercalate "\n  " (extractPNode <$> vertices phy)
      <> "\n\n  "
      <> intercalate "\n  " (filterMap extractEdge $ edges phy)
      <> "\n}"
