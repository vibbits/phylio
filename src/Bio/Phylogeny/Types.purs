module Bio.Phylogeny.Types where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.State (State, evalState, get, modify)
import Data.Array ((:))
import Data.Array as A
import Data.Enum (succ)
import Data.Foldable (class Foldable, foldMap, foldr, foldl, foldrDefault)
import Data.Graph (Graph, fromMap, outEdges, topologicalSort, vertices)
import Data.Identity (Identity)
import Data.Interpolate (i)
import Data.List (List(Nil))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (class Newtype)
import Data.Number as N
import Data.Number.Format as NF
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Text.Parsing.Parser as TPP
import Text.Parsing.Parser.Pos as Pos

data Attribute
  = Numeric Number
  | Text String
  | Bool Boolean

derive instance eqAttribute :: Eq Attribute

data NodeType
  = Clade
  | Taxa
  | Hybrid
  | LateralGeneTransfer
  | Recombination

derive instance eqNodeType :: Eq NodeType

nodeTypeToString :: NodeType -> String
nodeTypeToString Clade = "Clade"
nodeTypeToString Taxa = "Taxa"
nodeTypeToString Hybrid = "Hybrid"
nodeTypeToString LateralGeneTransfer = "LateralGeneTransfer"
nodeTypeToString Recombination = "Recombination"

type NodeName = String

type NodeIdentifier = Int

type PNodeInternal =
  { name :: NodeName
  , node :: NodeType
  , branchLength :: Number
  , ref :: Maybe Int
  , attributes :: M.Map String Attribute
  }

newtype PNode = PNode PNodeInternal

derive instance newtypePNode :: Newtype PNode _

instance eqPNode :: Eq PNode where
  eq (PNode a) (PNode b) =
    a.name == b.name && a.node == b.node && a.branchLength == b.branchLength && a.attributes == b.attributes

instance ordPNode :: Ord PNode where
  compare (PNode a) (PNode b) =
    compare a.name b.name

newtype Network = Network (Graph NodeIdentifier PNode)

derive instance newtypeNetwork :: Newtype Network _

instance eqNetwork :: Eq Network where
  eq (Network a) (Network b) = (vertices a == vertices b) && (edges a == edges b)
    where
    edges :: Graph NodeIdentifier PNode -> List (Tuple NodeIdentifier NodeIdentifier)
    edges graph = (topologicalSort graph) >>= (\id -> (id /\ _) <$> fromMaybe Nil (outEdges id graph))

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
  , network :: M.Map NodeIdentifier (Tuple PNode (List NodeIdentifier))
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

instance showAttribute :: Show Attribute where
  show (Numeric n) = show n
  show (Text s) = s
  show (Bool b) = show b

attributeToString :: Attribute -> String
attributeToString (Text s) = s
attributeToString (Bool b) = show b
attributeToString (Numeric n) = NF.toString n

parseAttribute :: String -> Attribute
parseAttribute attr =
  case N.fromString attr of
    Just n -> Numeric n
    _ -> case attr of
      "true" -> Bool true
      "false" -> Bool false
      _ -> Text attr

instance showPNode :: Show PNode where
  show (PNode { name, node, branchLength, ref, attributes }) =
    i "PNode{name=" name ", type=" (nodeTypeToString node) ", BL=" (show branchLength) ", #" (show ref) ", attrs=" (show attributes) "}"

instance showGraph :: Show Network where
  show (Network g) = show $ A.fromFoldable $ topologicalSort g

newtype Position =
  Position
    { column :: Int
    , line :: Int
    }

derive instance eqPosition :: Eq Position

instance showPosition :: Show Position where
  show (Position pos) =
    "line: " <> show pos.line <> ", column: " <> show pos.column

data ParseError =
  ParseError String Position

derive instance eqParseError :: Eq ParseError

instance showParseError :: Show ParseError where
  show (ParseError msg pos) = "ParseError: " <> msg <> " @ " <> show pos

toParseError :: TPP.ParseError -> ParseError
toParseError (TPP.ParseError msg (Pos.Position { column, line })) =
  ParseError msg (Position { column: column, line: line })

-- This is an intermediate representation for a Network
data Tree a
  = Leaf a
  | Internal a (Array (Tree a))
  | Empty (M.Map String Attribute)

derive instance eqTree :: Eq a => Eq (Tree a)

instance functorTree :: Functor Tree where
  map f (Leaf n) = Leaf (f n)
  map f (Internal p cs) = Internal (f p) (map (map f) cs)
  map _ (Empty x) = Empty x

instance semigroupTree :: Semigroup (Tree a) where
  append (Empty x) (Empty y) = Empty (M.union x y)
  append (Empty _) n = n
  append (Leaf l) c = Internal l [ c ]
  append (Internal n cs) c = Internal n (c : cs)

instance monoidTree :: Monoid (Tree a) where
  mempty = Empty M.empty

-- | Pre-order tree traversal
instance foldableTree :: Foldable Tree where
  foldl f acc (Leaf n) = f acc n
  foldl f acc (Internal p cs) = foldl (foldl f) (f acc p) cs
  foldl _ acc (Empty _) = acc
  foldMap f (Leaf n) = f n
  foldMap f (Internal p cs) = f p <> foldMap (foldMap f) cs
  foldMap _ (Empty _) = mempty
  foldr f = foldrDefault f

instance traversableTree :: Traversable Tree where
  traverse action (Leaf n) = Leaf <$> action n
  traverse action (Internal p cs) = Internal <$> action p <*> traverse (traverse action) cs
  traverse _ (Empty x) = pure (Empty x)
  sequence = sequenceDefault

instance showTree :: Show a => Show (Tree a) where
  show (Leaf n) = "Leaf(" <> show n <> ")"
  show (Internal p cs) = "Internal(" <> show p <> ", " <> show cs <> ")"
  show (Empty x) = "Empty(" <> show x <> ")"

type Parser a = TPP.ParserT String Identity a

getRef :: PNode -> Maybe Int
getRef (PNode { ref }) = ref

maxRef :: Tree PNode -> Maybe Int
maxRef =
  foldl
    ( \acc n ->
        case getRef n of
          Nothing -> acc
          Just b -> (max b <$> acc) <|> Just b
    )
    Nothing

interpretIntermediate :: Int -> Tree PNode -> PartialPhylogeny
interpretIntermediate refOffset tree =
  let
    ancestor :: Tree PNode -> Maybe PNode
    ancestor (Leaf l) = Just l
    ancestor (Internal p _) = Just p
    ancestor (Empty _) = Nothing

    startRef :: Int
    startRef = fromMaybe refOffset $ (_ + 1) <$> maxRef tree

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
    children (Empty _) = M.empty
    children (Leaf (PNode { ref })) =
      case ref of
        Just r -> M.singleton r Nil
        Nothing -> M.empty
    children (Internal (PNode { ref }) cs) =
      case ref of
        Just r -> foldl
          (\acc t -> M.unionWith (L.union) acc $ children t)
          (M.singleton r (L.fromFoldable $ A.catMaybes $ (\t -> getRef =<< ancestor t) <$> cs))
          cs
        Nothing -> foldl (\acc t -> M.unionWith (L.union) acc $ children t) M.empty cs

    children' = children tagged

    foldFn n@(PNode { ref }) graph =
      case ref of
        Just r -> [ (r /\ (n /\ (fromMaybe Nil $ M.lookup r children'))) ] <> graph
        Nothing -> graph

    inMeta :: Int -> Maybe Metadata
    inMeta parent =
      Just
        { name: Nothing
        , parent: parent
        , rooted: false
        , description: Nothing
        }

  in
    PartialPhylogeny
      { metadata: A.fromFoldable (ancestor tagged >>= getRef >>= inMeta)
      , network: M.fromFoldable $ foldr foldFn [] tagged
      , maxRef: fromMaybe refOffset $ maxRef tagged
      }

toPhylogeny :: PartialPhylogeny -> Phylogeny
toPhylogeny (PartialPhylogeny phylogeny) =
  { metadata: phylogeny.metadata
  , network: Network $ fromMap phylogeny.network
  }
