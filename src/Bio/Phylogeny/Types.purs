module Bio.Phylogeny.Types where

import Prelude

import Control.Monad.State (State, evalState, get, modify)
import Data.Array as A
import Data.Enum (succ)
import Data.Foldable (class Foldable, foldMap, foldr, foldl, foldrDefault, maximum)
import Data.Graph (Graph, fromMap, outEdges, topologicalSort, vertices)
import Data.Identity (Identity)
import Data.Interpolate (i)
import Data.List (List(Nil))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Text.Parsing.Parser (ParserT)

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
  , attributes :: M.Map String Attribute
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
  show (Network g) = show $ A.fromFoldable $ topologicalSort g

-- This is an intermediate representation for a Network
data Tree a
  = Leaf a
  | Internal a (Array (Tree a))

derive instance eqTree :: Eq a => Eq (Tree a)

instance functorTree :: Functor Tree where
  map f (Leaf n) = Leaf (f n)
  map f (Internal p cs) = Internal (f p) (map (map f) cs)

-- | Pre-order tree traversal
instance foldableTree :: Foldable Tree where
  foldl f acc (Leaf n) = f acc n
  foldl f acc (Internal p cs) = foldl (foldl f) (f acc p) cs
  foldMap f (Leaf n) = f n
  foldMap f (Internal p cs) = f p <> foldMap (foldMap f) cs
  foldr f = foldrDefault f

instance traverseNewickTree :: Traversable Tree where
  traverse action (Leaf n) = Leaf <$> action n
  traverse action (Internal p cs) = Internal <$> action p <*> traverse (traverse action) cs
  sequence = sequenceDefault

instance showNewickTree :: Show a => Show (Tree a) where
  show (Leaf n) = "Leaf(" <> show n <> ")"
  show (Internal p cs) = "Internal(" <> show p <> ", " <> show cs <> ")"

type Parser a = ParserT String Identity a

interpretIntermediate :: Tree PNode -> Phylogeny
interpretIntermediate tree =
  let
    ancestor :: Tree PNode -> PNode
    ancestor (Leaf l) = l
    ancestor (Internal p _) = p

    getRef :: PNode -> Maybe Int
    getRef (PNode { ref }) = ref

    startRef :: Int
    startRef =
      fromMaybe 0 $ (_ + 1) <$> maxRef
      where
      maxRef :: Maybe Int
      maxRef = maximum $ A.catMaybes $ getRef
        <$> foldl (\a b -> a <> [ b ]) [] tree

    postIncrementRef :: State Int Int
    postIncrementRef = do
      ref <- get
      _ <- modify $ fromMaybe 0 <<< succ
      pure ref

    assignRef :: PNode -> State Int PNode
    assignRef pnode@(PNode n) =
      if isJust n.ref then
        pure pnode
      else do
        ref <- postIncrementRef
        pure $ PNode (n { ref = Just ref })

    tagged :: Tree PNode
    tagged = evalState (traverse assignRef tree) startRef

    children :: Tree PNode -> M.Map Int (List Int)
    children (Leaf (PNode { ref })) =
      case ref of
        Just r -> M.singleton r Nil
        Nothing -> M.empty
    children (Internal (PNode { ref }) cs) =
      case ref of
        Just r -> foldl
          (\acc t -> M.unionWith (L.union) acc $ children t)
          (M.singleton r (L.fromFoldable $ A.catMaybes $ getRef <<< ancestor <$> cs))
          cs
        Nothing -> foldl (\acc t -> M.unionWith (L.union) acc $ children t) M.empty cs

    children' = children tagged

    foldFn n@(PNode { ref }) graph =
      case ref of
        Just r -> [ (r /\ (n /\ (fromMaybe Nil $ M.lookup r children'))) ] <> graph
        Nothing -> graph

  in
    { root: fromMaybe 0 $ getRef $ ancestor tagged
    , network: Network $ fromMap $ M.fromFoldable $ foldr (foldFn) [] tagged
    }
