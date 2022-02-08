module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Bio.Phylogeny.Newick as Newick
import Test.Bio.Phylogeny.Nexus as Nexus
import Test.Bio.Phylogeny.PhyloXml as PhyloXml
import Test.Compare as Compare
import Test.Spec (Spec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

specs :: Spec Unit
specs = do
  Newick.specs
  Nexus.specs
  PhyloXml.specs
  Compare.specs

main :: Effect Unit
main = launchAff_ do
  runSpec [ consoleReporter ] specs
