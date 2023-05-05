module JsLib (parse, Result) where

import Prelude

import Bio.Phylogeny as Phylio
import Bio.Phylogeny.Internal.Attributes (Attribute(..))
import Bio.Phylogeny.Internal.Types (PhylogenyNode(..), eventToString)
import Control.Alternative ((<|>))
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Function.Uncurried (Fn0, Fn5, runFn0, runFn5)
import Data.Map (Map, toUnfoldableUnordered)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

foreign import data Pair :: Type
foreign import data Attr :: Type
foreign import data AttrMap :: Type
foreign import data Metadata :: Type
foreign import data Taxa :: Type
foreign import data Edge :: Type
foreign import data Result :: Type

foreign import text :: String -> Attr
foreign import numeric :: Number -> Attr
foreign import bool :: Boolean -> Attr
foreign import list :: Array Attr -> Attr
foreign import mapping :: Array Pair -> Attr

foreign import mkPair :: String -> Attr -> Pair
foreign import mkMap :: Fn0 AttrMap
foreign import addKeyToMap :: AttrMap -> String -> Attr -> AttrMap
foreign import _mkEdge :: Int -> Int -> Edge
foreign import _mkTaxa :: Fn5 String String Number Int AttrMap Taxa
foreign import _mkMeta :: String -> String -> Int -> Boolean -> Metadata
foreign import _mkPhylogeny :: Array Metadata -> Array Taxa -> Array Edge -> Result

foreign import _mkError :: String -> Result

attrToForeign :: Attribute -> Attr
attrToForeign (Text t) = text t
attrToForeign (Numeric n) = numeric n
attrToForeign (Bool b) = bool b
attrToForeign (List as) = list $ attrToForeign <$> as
attrToForeign (Mapping m) = mapping
  ((\(k /\ v) -> mkPair k $ attrToForeign v) <$> toUnfoldableUnordered m)

-- Convert an attribute map into a javascript Map
attrsToMap :: Map String Attribute -> AttrMap
attrsToMap attrs =
  foldrWithIndex gen (runFn0 mkMap) attrs
  where
  gen :: String -> Attribute -> AttrMap -> AttrMap
  gen key attr map =
    addKeyToMap map key $ attrToForeign attr

metadata :: Phylio.Phylogeny -> Array Metadata
metadata phylogeny = mkMeta <$> Phylio.meta phylogeny
  where
  mkMeta :: Phylio.Metadata -> Metadata
  mkMeta m = _mkMeta (fromMaybe "" m.name) (fromMaybe "" m.description) m.parent m.rooted

edges :: Phylio.Phylogeny -> Array Edge
edges phylogeny =
  mkEdge <$> Phylio.edges phylogeny
  where
  mkEdge :: Tuple Int Int -> Edge
  mkEdge (from /\ to) = _mkEdge from to

vertices :: Phylio.Phylogeny -> Array Taxa
vertices phylogeny =
  mkTaxa <$> Phylio.vertices phylogeny
  where
  mkTaxa :: Phylio.Taxa -> Taxa
  mkTaxa (PhylogenyNode n) =
    runFn5
      _mkTaxa
      n.name
      (eventToString n.event)
      n.branchLength
      n.ref
      (attrsToMap n.attributes)

parse :: String -> Result
parse input =
  case parsed of
    Right phylogeny -> _mkPhylogeny (metadata phylogeny) (vertices phylogeny) (edges phylogeny)
    Left err -> _mkError $ Phylio.reportError input err
  where
  parsed = Phylio.parsePhyloXml input
    <|> Phylio.parseNewick input
    <|> Phylio.parseNexus input
