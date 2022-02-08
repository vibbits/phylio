module Test.Path where

import Node.Path (FilePath, concat)

append :: FilePath -> FilePath -> FilePath
append a b = concat [ a, b ]

infixr 5 append as </>

testDir :: FilePath
testDir = concat [ "test", "data" ]
