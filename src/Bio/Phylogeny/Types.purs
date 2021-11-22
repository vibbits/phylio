module Bio.Phylogeny.Types where

import Prelude

import Data.Array (fromFoldable)
import Data.Graph (Graph, outEdges, topologicalSort, vertices)
import Data.Interpolate (i)
import Data.List (List(Nil))
import Data.Map (Map)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

data Attribute
  = Numeric Number
  | Text String

derive instance eqAttribute :: Eq Attribute

data NodeType
  = Clade
  | Taxa
  | Hybrid
  | LateralGeneTransfer
  | Recombination

derive instance eqNodeType :: Eq NodeType

type NodeName = String

type NodeIdentifier = Int

newtype PNode = PNode
  { name :: NodeName
  , node :: NodeType
  , branchLength :: Number
  , ref :: Maybe Int
  , attributes :: Map String Attribute
  }

derive instance newtypePNode :: Newtype PNode _

derive instance eqPNode :: Eq PNode

newtype Network = Network (Graph NodeIdentifier PNode)

derive instance newtypeNetwork :: Newtype Network _

instance eqNetwork :: Eq Network where
  eq (Network a) (Network b) = (vertices a == vertices b) && (edges a == edges b)
    where
    edges :: Graph NodeIdentifier PNode -> List (Tuple NodeIdentifier NodeIdentifier)
    edges graph = (topologicalSort graph) >>= (\id -> (id /\ _) <$> fromMaybe Nil (outEdges id graph))

type Phylogeny =
  { root :: NodeIdentifier
  , network :: Network
  }

instance showAttribute :: Show Attribute where
  show (Numeric n) = show n
  show (Text s) = s

instance showNodeType :: Show NodeType where
  show Clade = "Clade"
  show Taxa = "Taxa"
  show Hybrid = "Hybrid"
  show LateralGeneTransfer = "LateralGeneTransfer"
  show Recombination = "Recombination"

instance showPNode :: Show PNode where
  show (PNode { name, node, branchLength, ref, attributes }) =
    i "PNode{" name ", " (show node) ", " (show branchLength) ", " (show ref) ", " (show attributes) "}"

instance showGraph :: Show Network where
  show (Network g) = show $ fromFoldable $ topologicalSort g
