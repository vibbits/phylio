module Bio.Phylogeny.Types where

import Prelude

import Data.Array (fromFoldable)
import Data.Graph (Graph, topologicalSort)
import Data.Interpolate (i)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

data Attribute
  = Numeric Number
  | Text String

data NodeType
  = Clade
  | Taxa
  | Hybrid
  | LateralGeneTransfer
  | Recombination

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

newtype Network = Network (Graph NodeIdentifier PNode)

derive instance newtypeNetwork :: Newtype Network _

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
