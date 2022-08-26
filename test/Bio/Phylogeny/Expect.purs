module Test.Bio.Phylogeny.Expect where

import Prelude

import Bio.Phylogeny (Phylogeny(..))
import Bio.Phylogeny.Internal.Types
  ( NodeIdentifier
  , PhylogenyNode(..)
  )
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Graph (Graph, lookup, topologicalSort, vertices)
import Data.List (catMaybes, length)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect.Exception (Error)
import Parsing (ParseError(..))
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Assertions.String (shouldContain)

graph :: Phylogeny -> Graph NodeIdentifier PhylogenyNode
graph (Phylogeny phy) = phy.network

nodeName :: PhylogenyNode -> Tuple String Number
nodeName (PhylogenyNode { name, branchLength }) = (name /\ branchLength)

nameNodes :: Phylogeny -> Array (Tuple String Number)
nameNodes phy =
  fromFoldable $ nodeName <$> (catMaybes $ flip lookup g <$> topologicalSort g)
  where
  g = graph phy

expectNNodes :: forall m. MonadThrow Error m => Either ParseError Phylogeny -> Int -> m Unit
expectNNodes input n =
  (length <<< vertices <<< graph <$> input)
    `shouldEqual`
      Right n

expectNames
  :: forall m
   . MonadThrow Error m
  => Either ParseError Phylogeny
  -> Array (Tuple String Number)
  -> m Unit
expectNames input names =
  (nameNodes <$> input)
    `shouldEqual`
      Right names

expectFail :: forall m. MonadThrow Error m => String -> Either ParseError Phylogeny -> m Unit
expectFail message input =
  case input of
    Left (ParseError errm _) -> errm `shouldContain` message
    Right _ -> fail "Expected parser failure"
