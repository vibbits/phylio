module Bio.Phylogeny.Parser where

import Bio.Phylogeny.Types (Attribute(..), Network(..), NodeIdentifier, NodeType(..), PNode(..), Phylogeny)
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
import Data.Number as N
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Prelude (class Functor, class Show, bind, const, discard, map, pure, show, ($), (*>), (+), (<$>), (<*), (<*>), (<<<), (<>))
import Text.Parsing.Parser (ParserT, fail, runParser)
import Text.Parsing.Parser.Combinators (between, many1, optional, optionMaybe, sepBy, try)
import Text.Parsing.Parser.String (char, oneOf, skipSpaces, string)
import Text.Parsing.Parser.Token (digit, letter)

-- This is the intermediate representation for Newick
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

type Parser a = ParserT String Identity a

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

node :: NodeType -> Parser PNode
node nt =
  let
    extras = [ '.', '_' ]
  in
    do
      skipSpaces
      name' <- fromCharArray <$> A.many (letter <|> digit <|> oneOf extras)
      ref <- optionMaybe refp
      len <- length <|> pure 0.0
      skipSpaces
      attrs <- attributes <|> pure M.empty
      skipSpaces
      case ref of
        Just (id /\ nodet) -> pure $ PNode
          { name: name'
          , node: nodet
          , branchLength: len
          , attributes: attrs
          , ref: Just id
          }
        Nothing -> pure $ PNode
          { name: name'
          , node: nt
          , branchLength: len
          , attributes: attrs
          , ref: Nothing
          }

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

attributes :: Parser (M.Map String Attribute)
attributes = do
  kvs <- between (string "[&&NHX:") (string "]") (attr `sepBy` char ':')
  pure $ M.fromFoldable kvs

attr :: Parser (Tuple String Attribute)
attr = do
  key <- fromCharArray <<< A.fromFoldable <$> many1 letter
  _ <- char '='
  val <- fromCharArray <<< A.fromFoldable <$> many1 (letter <|> digit <|> oneOf [ '.' ])
  case N.fromString val of
    Just value -> pure (key /\ Numeric value)
    Nothing -> pure (key /\ Text val)

leaf :: Parser (NewickTree PNode)
leaf = Leaf <$> node Taxa

internal :: Parser (NewickTree PNode) -> Parser (NewickTree PNode)
internal pars = do
  lst <- between (string "(") (string ")") ((skipSpaces *> pars <* skipSpaces) `sepBy` char ',')
  parent <- try $ node Clade
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

    tagged :: NewickTree PNode
    tagged = evalState (traverse assignRef tree) startRef

    root :: NodeIdentifier
    root = fromMaybe 0 $ getRef $ ancestor tagged

    children :: NewickTree PNode -> M.Map Int (List Int)
    children (Leaf (PNode { ref })) =
      case ref of
        Just r -> M.singleton r Nil
        Nothing -> M.empty
    children (Internal (PNode { ref }) cs) =
      case ref of
        Just r -> foldl
          (\acc t -> M.unionWith (union) acc $ children t)
          (M.singleton r (L.fromFoldable $ A.catMaybes $ getRef <<< ancestor <$> cs))
          cs
        Nothing -> foldl (\acc t -> M.unionWith (union) acc $ children t) M.empty cs

    children' = children tagged

    foldFn n@(PNode { ref }) graph =
      case ref of
        Just r -> [ (r /\ (n /\ (fromMaybe Nil $ M.lookup r children'))) ] <> graph
        Nothing -> graph

  in
    { root: root
    , network: Network $ G.fromMap $ M.fromFoldable $ foldr (foldFn) [] tagged
    }
