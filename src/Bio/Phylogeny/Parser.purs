module Bio.Phylogeny.Parser where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.Monad.State (State, evalState, get, modify)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Enum (succ)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, foldrDefault, maximum)
import Data.Graph as G
import Data.Identity (Identity)
import Data.Int as I
import Data.Interpolate (i)
import Data.List (List(Nil), union)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (class Newtype)
import Data.Number as N
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Text.Parsing.Parser (ParserT, fail, runParser)
import Text.Parsing.Parser.Combinators (between, many1, optional, optionMaybe, sepBy, try)
import Text.Parsing.Parser.String (char, skipSpaces, string)
import Text.Parsing.Parser.Token (digit, letter)

data Attribute
  = Numeric Number
  | Text String

data NodeType
  = Clade
  | Taxa
  | Hybrid
  | LateralGeneTransfer
  | Recombination

type NodeName
  = String

type NodeIdentifier
  = Int

newtype PNode
  = PNode
  { name :: NodeName
  , node :: NodeType
  , branchLength :: Number
  , ref :: Maybe Int
  , attributes :: M.Map String Attribute
  }

newtype Network
  = Network (G.Graph NodeIdentifier PNode)

derive instance newtypeNetwork :: Newtype Network _

type Phylogeny
  = { root :: NodeIdentifier
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
  show (PNode { name, node, branchLength, ref, attributes }) = i "PNode{" name ", " (show node) ", " (show branchLength) ", " (show ref) ", " (show attributes) "}"

instance showGraph :: Show Network where
  show (Network g) = show $ A.fromFoldable $ G.topologicalSort g

-- This is the second-stage intermediate representation
type NewickNode
  = Tuple NodeIdentifier (Tuple PNode (List NodeIdentifier))

type NewickGraph
  = Array NewickNode

-- This is the first-stage intermediate representation
data NewickTree a
  = Leaf a
  | Internal a (Array (NewickTree a))

instance functorNewickTree :: Functor NewickTree where
  map f (Leaf n) = Leaf (f n)
  map f (Internal p cs) = Internal (f p) (map (map f) cs)

-- | Pre-order tree traversal
instance foldableNewickTree :: Foldable NewickTree where
  foldl f acc (Leaf n) = f acc n
  foldl f acc (Internal p cs) = foldl (foldl f) (f acc p) cs
  foldMap f (Leaf n) = f n
  foldMap f (Internal p cs) = f p <> foldMap (foldMap f) cs
  foldr f = foldrDefault f

instance traverseNewickTree :: Traversable NewickTree where
  traverse action (Leaf n) = Leaf <$> action n
  traverse action (Internal p cs) = Internal <$> action p <*> traverse (traverse action) cs
  sequence = sequenceDefault

instance showNewickTree :: Show a => Show (NewickTree a) where
  show (Leaf n) = "Leaf(" <> show n <> ")"
  show (Internal p cs) = "Internal(" <> show p <> ", " <> show cs <> ")"

type Parser a
  = ParserT String Identity a

-- | Parse phylogenies serialised to the legacy Newick format
parseNewick :: String -> Either String Phylogeny
parseNewick input = lmap show $ runParser input newickParser

newickParser :: Parser Phylogeny
newickParser = do
  tree <- subTree <* char ';'
  pure $ interpretIntermediate tree

subTree :: Parser (NewickTree PNode)
subTree = fix $ \p -> internal p <|> leaf

refp :: Parser (Tuple Int NodeType)
refp = do
  _ <- char '#'
  t <-
    (const Hybrid <$> string "H")
      <|> (const LateralGeneTransfer <$> string "LGT")
      <|> (const Recombination <$> string "R")
      <|> pure Hybrid
  n <- fromCharArray <<< A.fromFoldable <$> many1 digit
  case I.fromString n of
    Just r -> pure (r /\ t)
    Nothing -> fail $ i "References must be an integer: " n

namep :: NodeType -> Parser PNode
namep nt = do
  skipSpaces
  name' <- fromCharArray <$> A.many letter
  ref <- optionMaybe refp
  len <- length <|> pure 0.0
  skipSpaces
  case ref of
    Just (id /\ nodet) -> pure $ PNode { name: name', node: nodet, branchLength: len, attributes: M.empty, ref: Just id }
    Nothing -> pure $ PNode { name: name', node: nt, branchLength: len, attributes: M.empty, ref: Nothing }

number :: Parser Number
number = do
  a <- fromCharArray <<< A.fromFoldable <$> many1 digit
  _ <- optional $ char '.'
  b <- fromCharArray <<< A.fromFoldable <$> A.many digit
  case N.fromString $ a <> "." <> b of
    Nothing -> fail "Not a number"
    Just num -> pure num

length :: Parser Number
length = char ':' *> number

leaf :: Parser (NewickTree PNode)
leaf = Leaf <$> namep Taxa

internal :: Parser (NewickTree PNode) -> Parser (NewickTree PNode)
internal pars = do
  lst <- between (string "(") (string ")") (pars `sepBy` char ',')
  parent <- try $ namep Clade
  pure $ Internal parent $ A.fromFoldable lst

ancestor :: NewickTree PNode -> PNode
ancestor (Leaf l) = l
ancestor (Internal p _) = p

getRef :: PNode -> Maybe Int
getRef (PNode { ref }) = ref
  
interpretIntermediate :: NewickTree PNode -> Phylogeny
interpretIntermediate tree =
  let
    startRef :: Int
    startRef =
      fromMaybe 0                                    -- default to 0 if there are no pre-assigned refs
        $ (_ + 1)                                    -- add 1 to this ref
        <$> ( maximum                                -- Start assigning refs to nodes from the max ref from Newick + 1
              $ A.catMaybes                          -- Only keep Just values, nodes with assigned refs in the Newick description
              $ getRef                               -- Extract references (:: Maybe Int)
              <$> foldl (\a b -> a <> [ b ]) [] tree -- Preorder nodes from the tree in an array
          )

    postIncrementRef :: State Int Int
    postIncrementRef = do
      ref <- get
      _ <- modify $ fromMaybe 0 <<< succ
      pure ref

    assignRef :: PNode -> State Int PNode
    assignRef pnode@(PNode node) =
      if isJust node.ref then
        pure pnode
      else do
        ref <- postIncrementRef
        pure $ PNode (node { ref = Just ref })

    tagged :: NewickTree PNode
    tagged = evalState (traverse assignRef tree) startRef

    root :: NodeIdentifier
    root = fromMaybe 0 $ getRef $ ancestor tagged

    children :: NewickTree PNode -> M.Map Int (List Int)
    children (Leaf (PNode { ref })) =
      case ref of
        Just r -> M.singleton r Nil
        Nothing -> M.empty
    children (Internal (PNode {ref}) cs) =
      case ref of
           Just r -> foldl
                       (\acc t -> M.unionWith (union) acc $ children t)
                       (M.singleton r (L.fromFoldable $ A.catMaybes $ getRef <<< ancestor <$> cs))
                       cs
           Nothing -> foldl (\acc t -> M.unionWith (union) acc $ children t) M.empty cs

    children' = children tagged

    foldFn :: PNode -> NewickGraph -> NewickGraph
    foldFn node@(PNode {ref}) graph =
      case ref of
        Just r -> [(r /\ (node /\ (fromMaybe Nil $ M.lookup r children')))] <> graph
        Nothing -> graph
      
  in
    { root: root, network: Network $ G.fromMap $ M.fromFoldable $ foldr (foldFn) [] tagged }
