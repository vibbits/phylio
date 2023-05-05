module Bio.Phylogeny.Internal.Types
  ( Event(..)
  , eventToString
  , PhylogenyNode(..)
  , NodeIdentifier
  , NodeName
  , Network
  , Metadata
  , Phylogeny
  , PartialPhylogeny(..)
  , PartialNode
  , Parser
  , Tree(..)
  , interpretIntermediate
  , toPhylogeny
  , toAnnotatedPhylogeny
  ) where

import Prelude

import Bio.Phylogeny.Internal.Attributes (Attribute)
import Control.Alt ((<|>))
import Control.Monad.State (State, evalState, get, modify)
import Data.Array ((:))
import Data.Array as A
import Data.Enum (succ)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, foldrDefault)
import Data.Graph (Graph, fromMap)
import Data.Identity (Identity)
import Data.List (List(Nil))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Parsing (ParserT)

data Event
  = Clade
  | Taxa
  | Hybrid
  | LateralGeneTransfer
  | Recombination

derive instance eqEvent :: Eq Event

eventToString :: Event -> String
eventToString Clade = "Clade"
eventToString Taxa = "Taxa"
eventToString Hybrid = "Hybrid"
eventToString LateralGeneTransfer = "LateralGeneTransfer"
eventToString Recombination = "Recombination"

instance showEvent :: Show Event where
  show = eventToString

type NodeName = String

type NodeIdentifier = Int

type PartialNode =
  { name :: NodeName
  , event :: Event
  , branchLength :: Number
  , ref :: Maybe NodeIdentifier
  , attributes :: M.Map String Attribute
  }

newtype PhylogenyNode = PhylogenyNode
  { name :: NodeName
  , event :: Event
  , branchLength :: Number
  , ref :: NodeIdentifier
  , attributes :: M.Map String Attribute
  }

instance eqPhylogenyNode :: Eq PhylogenyNode where
  eq (PhylogenyNode a) (PhylogenyNode b) =
    a.name == b.name && a.event == b.event && a.branchLength == b.branchLength && a.attributes ==
      b.attributes

instance ordPhylogenyNode :: Ord PhylogenyNode where
  compare (PhylogenyNode a) (PhylogenyNode b) =
    compare a.name b.name

instance showPhylogenyNode :: Show PhylogenyNode where
  show _ = "PhylogenyNode"

phylogenyNode :: PartialNode -> NodeIdentifier -> PhylogenyNode
phylogenyNode partial ref =
  PhylogenyNode
    { name: partial.name
    , event: partial.event
    , branchLength: partial.branchLength
    , ref: ref
    , attributes: partial.attributes
    }

type Network = Graph NodeIdentifier PhylogenyNode

type Metadata =
  { name :: Maybe String
  , parent :: NodeIdentifier
  , rooted :: Boolean
  , description :: Maybe String
  }

type Phylogeny =
  { metadata :: Array Metadata
  , network :: Network
  }

newtype PartialPhylogeny = PartialPhylogeny
  { metadata :: Array Metadata
  , network :: M.Map NodeIdentifier (Tuple PhylogenyNode (List NodeIdentifier))
  , maxRef :: NodeIdentifier
  }

instance semigroupPartialPhylogeny :: Semigroup PartialPhylogeny where
  append (PartialPhylogeny a) (PartialPhylogeny b) =
    PartialPhylogeny
      { metadata: a.metadata <> b.metadata
      , network: M.union a.network b.network
      , maxRef: max a.maxRef b.maxRef
      }

instance monoidPartialPhylogeny :: Monoid PartialPhylogeny where
  mempty =
    PartialPhylogeny
      { metadata: []
      , network: M.empty
      , maxRef: 0
      }

-- This is an intermediate representation for a Network
data Tree a
  = Leaf a
  | Internal a (Array (Tree a))

derive instance eqTree :: Eq a => Eq (Tree a)

instance showTree :: Show a => Show (Tree a) where
  show (Leaf l) = "L: " <> show l
  show (Internal v children) = "(I:" <> show v <> toStr children <> ")"
    where
    toStr :: forall x. Show x => Array x -> String
    toStr xs = foldl (\acc x -> acc <> " " <> show x) "" xs

instance functorTree :: Functor Tree where
  map f (Leaf n) = Leaf (f n)
  map f (Internal p cs) = Internal (f p) (map (map f) cs)

instance semigroupTree :: Semigroup (Tree a) where
  append (Leaf l) c = Internal l [ c ]
  append (Internal n cs) c = Internal n (c : cs)

-- | Pre-order tree traversal
instance foldableTree :: Foldable Tree where
  foldl f acc (Leaf n) = f acc n
  foldl f acc (Internal p cs) = foldl (foldl f) (f acc p) cs
  foldMap f (Leaf n) = f n
  foldMap f (Internal p cs) = f p <> foldMap (foldMap f) cs
  foldr f = foldrDefault f

instance traversableTree :: Traversable Tree where
  traverse action (Leaf n) = Leaf <$> action n
  traverse action (Internal p cs) = Internal <$> action p <*> traverse (traverse action) cs
  sequence = sequenceDefault

type Parser a = ParserT String Identity a

maxRef :: Tree PartialNode -> Maybe Int
maxRef =
  foldl
    ( \acc n ->
        case n.ref of
          Nothing -> acc
          Just b -> (max b <$> acc) <|> Just b
    )
    Nothing

interpretIntermediate :: Int -> Tree PartialNode -> PartialPhylogeny
interpretIntermediate refOffset tree =
  let
    ancestor :: Tree PhylogenyNode -> PhylogenyNode
    ancestor (Leaf l) = l
    ancestor (Internal p _) = p

    startRef :: Int
    startRef = fromMaybe refOffset $ (_ + 1) <$> maxRef tree

    postIncrementRef :: State Int Int
    postIncrementRef = do
      ref <- get
      _ <- modify $ fromMaybe 0 <<< succ
      pure ref

    assignRef :: PartialNode -> State Int PhylogenyNode
    assignRef pnode =
      case pnode.ref of
        Just ref -> pure $ phylogenyNode pnode ref
        _ -> do
          ref <- postIncrementRef
          pure $ phylogenyNode pnode ref

    tagged :: Tree PhylogenyNode
    tagged = evalState (traverse assignRef tree) startRef

    getRef :: PhylogenyNode -> Int
    getRef (PhylogenyNode n) = n.ref

    children :: Tree PhylogenyNode -> M.Map Int (List Int)
    children (Leaf (PhylogenyNode { ref })) = M.singleton ref Nil
    children (Internal (PhylogenyNode { ref }) cs) =
      foldl
        (\acc t -> M.unionWith (L.union) acc $ children t)
        (M.singleton ref (L.fromFoldable $ (getRef <<< ancestor) <$> cs))
        cs

    foldFn
      :: PhylogenyNode
      -> Array (Tuple NodeIdentifier (Tuple PhylogenyNode (List Int)))
      -> Array (Tuple NodeIdentifier (Tuple PhylogenyNode (List Int)))
    foldFn n@(PhylogenyNode { ref }) graph =
      [ (ref /\ (n /\ (fromMaybe Nil $ M.lookup ref $ children tagged))) ] <> graph

    inMeta :: Int -> Metadata
    inMeta parent =
      { name: Nothing
      , parent: parent
      , rooted: true
      , description: Nothing
      }

  in
    PartialPhylogeny
      { metadata: [ inMeta $ getRef $ ancestor tagged ]
      , network: M.fromFoldable $ foldr foldFn [] tagged
      , maxRef: foldl (\acc n -> max (getRef n) acc) 0 tagged
      }

toPhylogeny :: PartialPhylogeny -> Phylogeny
toPhylogeny (PartialPhylogeny phylogeny) =
  { metadata: phylogeny.metadata
  , network: fromMap phylogeny.network
  }

toAnnotatedPhylogeny :: Array Metadata -> PartialPhylogeny -> Phylogeny
toAnnotatedPhylogeny metadata (PartialPhylogeny phylogeny) =
  { metadata: mergeMetadata <$> A.zip metadata phylogeny.metadata
  , network: fromMap phylogeny.network
  }
  where
  mergeMetadata :: Tuple Metadata Metadata -> Metadata
  mergeMetadata (Tuple a b) =
    { name: a.name <|> b.name
    , parent: max a.parent b.parent
    , rooted: a.rooted || b.rooted
    , description: a.description <|> b.description
    }
