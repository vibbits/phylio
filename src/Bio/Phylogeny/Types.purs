module Bio.Phylogeny.Types where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.State (State, evalState, get, modify)
import Data.Array ((:), intercalate)
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Either (Either(Right))
import Data.Enum (succ)
import Data.Foldable (class Foldable, foldMap, foldr, foldl, foldrDefault)
import Data.Graph (Graph, fromMap, outEdges, topologicalSort, vertices)
import Data.Identity (Identity)
import Data.List (List(Nil))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (class Newtype)
import Data.Number as N
import Data.Number.Format as NF
import Data.String.Regex as RE
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Tuple (Tuple, uncurry)
import Data.Tuple.Nested ((/\))
import Parsing (ParserT)

data Attribute
  = Numeric Number
  | Text String
  | Bool Boolean
  | List (Array Attribute)
  | Mapping (M.Map String Attribute)

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
  show (Numeric n) = "Numeric: " <> show n
  show (Text s) = "Text: " <> s
  show (Bool b) = "Bool: " <> show b
  show (List as) = "List: " <> (show as)
  show (Mapping m) = "Mapping: {" <> (intercalate ", " ((\(k /\ v) -> (show k) <> ": " <> (show v)) <$> M.toUnfoldable m)) <> "}"

attributeToString :: Attribute -> String
attributeToString (Text s) = s
attributeToString (Bool b) = show b
attributeToString (Numeric n) = NF.toString n
attributeToString (List as) = show as
attributeToString (Mapping m) = show m

attributeToBool :: Attribute -> Maybe Boolean
attributeToBool (Bool b) = Just b
attributeToBool _ = Nothing

parseAttribute :: String -> Attribute
parseAttribute attr =
  -- If the string contains only numeric characters try to make a number
  -- This is because `fromString` will accept string that onlt *start* with numbers
  -- so a value like "25_BRACA" will become `Numeric 25` otherwise
  if (flip RE.test attr <$> RE.regex "^\\d*.?\\d*$" noFlags) == Right true then
    case N.fromString attr of
      Just num -> Numeric num
      _ -> Text attr
  else
    case attr of
      "true" -> Bool true
      "false" -> Bool false
      _ -> Text attr

instance showPNode :: Show PNode where
  show (PNode { name, node, branchLength, ref, attributes }) =
    ( "PNode{name=" <> name
        <> ", type="
        <> (nodeTypeToString node)
        <> ", BL="
        <> (show branchLength)
        <> ", #"
        <> (show ref)
        <> ", attrs="
        <> (show attributes)
        <> "}"
    )

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

instance showTree :: Show a => Show (Tree a) where
  show (Leaf n) = "Leaf(" <> show n <> ")"
  show (Internal p cs) = "Internal(" <> show p <> ", " <> show cs <> ")"

type Parser a = ParserT String Identity a

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
    ancestor (Internal p _) = Just p --TODO: This doesn't need to be a Maybe?

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
        , rooted: true
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

toAnnotatedPhylogeny :: Array Metadata -> PartialPhylogeny -> Phylogeny
toAnnotatedPhylogeny metadata (PartialPhylogeny phylogeny) =
  { metadata: uncurry mergeMetadata <$> A.zip metadata phylogeny.metadata
  , network: Network $ fromMap phylogeny.network
  }
  where
  mergeMetadata :: Metadata -> Metadata -> Metadata
  mergeMetadata a b =
    { name: a.name <|> b.name
    , parent: max a.parent b.parent
    , rooted: a.rooted || b.rooted
    , description: a.description <|> b.description
    }
