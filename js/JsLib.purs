module JsLib (edges, parse, vertices) where

import Prelude

import Bio.Phylogeny as Phylio
import Bio.Phylogeny.Internal.Attributes (Attribute(..))
import Bio.Phylogeny.Internal.Types (PhylogenyNode(..), eventToString)
import Control.Alternative ((<|>))
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Function.Uncurried (Fn0, Fn5, runFn0, runFn5)
import Data.Map (Map, toUnfoldableUnordered)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Foreign (Foreign)

foreign import text :: String -> Foreign
foreign import numeric :: Number -> Foreign
foreign import bool :: Boolean -> Foreign
foreign import list :: Array Foreign -> Foreign
foreign import mapping :: Array Foreign -> Foreign

foreign import mkPair :: String -> Foreign -> Foreign
foreign import mkMap :: Fn0 Foreign
foreign import addKeyToMap :: Foreign -> String -> Foreign -> Foreign
foreign import _mkEdge :: Int -> Int -> Foreign
foreign import _mkTaxa :: Fn5 String String Number Int Foreign Foreign

foreign import mkError :: String -> Foreign
foreign import mkSuccess :: forall a. a -> Foreign

attrToForeign :: Attribute -> Foreign
attrToForeign (Text t) = text t
attrToForeign (Numeric n) = numeric n
attrToForeign (Bool b) = bool b
attrToForeign (List as) = list $ attrToForeign <$> as
attrToForeign (Mapping m) = mapping
  ((\(k /\ v) -> mkPair k $ attrToForeign v) <$> toUnfoldableUnordered m)

-- Convert an attribute map into a javascript Map
attrsToMap :: Map String Attribute -> Foreign
attrsToMap attrs =
  foldrWithIndex gen (runFn0 mkMap) attrs
  where
  gen :: String -> Attribute -> Foreign -> Foreign
  gen key attr map =
    addKeyToMap map key $ attrToForeign attr

edges :: Phylio.Phylogeny -> Array Foreign
edges phylogeny =
  mkEdge <$> Phylio.edges phylogeny
  where
  mkEdge :: Tuple Int Int -> Foreign
  mkEdge (from /\ to) = _mkEdge from to

vertices :: Phylio.Phylogeny -> Array Foreign
vertices phylogeny =
  mkTaxa <$> Phylio.vertices phylogeny
  where
  mkTaxa :: Phylio.Taxa -> Foreign
  mkTaxa (PhylogenyNode n) =
    runFn5
      _mkTaxa
      n.name
      (eventToString n.event)
      n.branchLength
      n.ref
      (attrsToMap n.attributes)

parse :: String -> Foreign
parse input =
  case parsed of
    Right phylogeny -> mkSuccess phylogeny
    Left err -> mkError $ Phylio.reportError input err
  where
  parsed = Phylio.parsePhyloXml input
    <|> Phylio.parseNewick input
    <|> Phylio.parseNexus input
