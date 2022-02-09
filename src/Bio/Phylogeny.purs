module Bio.Phylogeny
  ( Taxa
  , Phylogeny(Phylogeny)
  , dot
  , roots
  , parseNewick
  , parseNexus
  , parsePhyloXml
  , reportError
  ) where

import Prelude

import Bio.Phylogeny.Newick (parseNewick) as Internal
import Bio.Phylogeny.Nexus (parseNexus) as Internal
import Bio.Phylogeny.PhyloXml (parsePhyloXml) as Internal
import Bio.Phylogeny.Types (Network(..), NodeIdentifier, PNode(..), ParseError(..), Phylogeny, Position(..)) as Internal
import Data.Array ((!!))
import Data.Array as A
import Data.Either (Either)
import Data.Filterable (filter, filterMap)
import Data.Graph as G
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Data.String.Utils (lines, repeat)
import Data.Traversable (intercalate, sequence)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

newtype Phylogeny = Phylogeny Internal.Phylogeny

-- Two graphs are equal if they have the same vertex set and the same set of edges.
instance eqPhylogeny :: Eq Phylogeny where
  eq a b =
    L.sort (edges a) == L.sort (edges b)
      && L.sort (vertices a) == L.sort (vertices b)
      && A.sort (roots' a) == A.sort (roots' b)

instance showPhylogeny :: Show Phylogeny where
  show (Phylogeny p) = show p

type Taxa = Internal.PNode

graph :: Phylogeny -> G.Graph Internal.NodeIdentifier Internal.PNode
graph (Phylogeny { network }) = un Internal.Network network

roots' :: Phylogeny -> Array Int
roots' (Phylogeny { metadata }) = (_.parent) <$> metadata

roots :: Phylogeny -> Array Taxa
roots phylogeny = A.catMaybes $ (flip G.lookup graph') <$> roots' phylogeny
  where
  graph' = graph phylogeny

edges :: Phylogeny -> List (Tuple Taxa Taxa)
edges phylogeny =
  vertices phylogeny >>= go
  where
  graph' :: G.Graph Internal.NodeIdentifier Internal.PNode
  graph' = graph phylogeny

  go :: Taxa -> List (Tuple Taxa Taxa)
  go node = case ref node of
    Nothing -> Nil
    Just idx -> (node /\ _) <$> L.catMaybes ((flip G.lookup graph') <$> (maybe Nil identity $ G.outEdges idx graph'))

vertices :: Phylogeny -> List Taxa
vertices phylogeny = G.vertices $ graph phylogeny

ref :: Taxa -> Maybe Int
ref (Internal.PNode node) = node.ref

-- Parsing
parseNewick :: String -> Either Internal.ParseError Phylogeny
parseNewick input = Phylogeny <$> Internal.parseNewick input

parsePhyloXml :: String -> Either Internal.ParseError Phylogeny
parsePhyloXml input = Phylogeny <$> Internal.parsePhyloXml input

parseNexus :: String -> Either Internal.ParseError Phylogeny
parseNexus input = Phylogeny <$> Internal.parseNexus input

reportError :: Internal.ParseError -> String -> String
reportError (Internal.ParseError msg (Internal.Position { line, column })) input =
  "ERROR: " <> msg <> "\n" <> offender <> "\n" <> location
  where
  offender = case lines input !! (line - 1) of
    Just l -> l
    Nothing -> "????"
  location = case repeat (column - 1) " " of
    Just padding -> padding <> "^"
    _ -> ""

dfTraverse :: Int -> Phylogeny -> L.List Taxa
dfTraverse root phylogeny =
  case A.find (_ == root) $ roots' phylogeny of
    Nothing -> Nil
    Just root' ->
      maybe Nil identity $ sequence $ flip G.lookup graph' <$> go Nil root'
  where
  graph' :: G.Graph Internal.NodeIdentifier Internal.PNode
  graph' = graph phylogeny

  go :: L.List Int -> Int -> L.List Int
  go visited parent =
    parent : maybe visited (\ids -> ids >>= go visited) (filter (flip L.notElem visited) <$> G.outEdges parent graph')

dot :: Phylogeny -> String
dot phy =
  let
    extractPNode :: Taxa -> String
    extractPNode tax@(Internal.PNode node) = case ref tax of
      Just ref' -> show ref' <> " [label=\"" <> node.name <> "\"];"
      Nothing -> "_" <> " [label=\"" <> node.name <> "\"];"

    extractEdge :: Tuple Taxa Taxa -> Maybe String
    extractEdge (from /\ to) = case ((ref from) /\ (ref to)) of
      ((Just from') /\ (Just to')) -> Just (show from' <> " -> " <> show to')
      _ -> Nothing

    vertices' :: List String
    vertices' = extractPNode <$> vertices phy

    edges' :: List String
    edges' = filterMap extractEdge $ edges phy
  in
    "strict digraph {\n\n  "
      <> intercalate "\n  " vertices'
      <> "\n\n  "
      <> intercalate "\n  " edges'
      <> "\n}"
