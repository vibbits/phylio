module Test.Bio.Phylogeny.Expect where

import Data.Graph (Graph, lookup, topologicalSort, vertices)
import Prelude

import Bio.Phylogeny.Types (Network(..), NodeIdentifier, PNode(..), Phylogeny)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.List (catMaybes, length)
import Data.Newtype (un)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect.Exception (Error)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Assertions.String (shouldContain)

graph :: Phylogeny -> Graph NodeIdentifier PNode
graph phy = un Network phy.network

nodeName :: PNode -> Tuple String Number
nodeName (PNode { name, branchLength }) = (name /\ branchLength)

nameNodes :: Phylogeny -> Array (Tuple String Number)
nameNodes phy =
  fromFoldable $ nodeName <$> (catMaybes $ flip lookup g <$> topologicalSort g)
  where
  g = graph phy

expectNNodes :: forall m. MonadThrow Error m => Either String Phylogeny -> Int -> m Unit
expectNNodes input n =
  (length <<< vertices <<< graph <$> input)
    `shouldEqual`
      Right n

expectNames :: forall m. MonadThrow Error m => Either String Phylogeny -> Array (Tuple String Number) -> m Unit
expectNames input names =
  (nameNodes <$> input)
    `shouldEqual`
      Right names

expectFail :: forall m. MonadThrow Error m => String -> Either String Phylogeny -> m Unit
expectFail message input =
  case input of
    Left errm -> errm `shouldContain` message
    Right _ -> fail "Expected parser failure"
