module Bio.Phylogeny.Internal.Nexus (parseNexus) where

import Prelude hiding (between)

import Bio.Phylogeny.Internal.Newick (subTree)
import Bio.Phylogeny.Internal.Types
  ( PartialNode
  , PartialPhylogeny(..)
  , Phylogeny
  , Tree
  , interpretIntermediate
  , toPhylogeny
  )
import Control.Alt ((<|>))
import Data.Array (many, fromFoldable)
import Data.Either (Either)
import Data.Filterable (filterMap)
import Data.Foldable (foldl)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.String (toUpper)
import Data.String.CodeUnits (fromCharArray)
import Parsing (ParserT, fail, runParser, ParseError)
import Parsing.Combinators (between, lookAhead, many1, manyTill)
import Parsing.String (anyChar, char, eof, string)
import Parsing.String.Basic (skipSpaces)
import Parsing.Token (digit, letter)

type Parser a = ParserT String Identity a

data Block = Block String (Maybe (Tree PartialNode))

parseNexus :: String -> Either ParseError Phylogeny
parseNexus input = runParser input nexusParser

nexusParser :: Parser Phylogeny
nexusParser =
  do
    _ <- string "#NEXUS"
    skipSpaces
    blocks <- many block
    skipSpaces
    eof
    case filterMap (\(Block _ phy) -> phy) blocks of
      [] -> fail "No valid phylogenies"
      trees ->
        pure
          $ toPhylogeny
          $ foldl (\acc@(PartialPhylogeny { maxRef }) ptree -> acc <> interpretIntermediate maxRef ptree) mempty trees

block :: Parser Block
block = between begin end blockInner

blockInner :: Parser Block
blockInner = do
  name <- fromCharArray <<< fromFoldable <$> (many1 letter <* char ';')
  skipSpaces
  phy <-
    if toUpper name == "TREES" then
      Just <$> tree
    else
      const Nothing <$> skipInner
  pure $ Block name phy

skipInner :: Parser Unit
skipInner = manyTill anyChar (lookAhead end) *> pure unit

begin :: Parser Unit
begin = (string "BEGIN" <|> string "Begin") *> skipSpaces

end :: Parser Unit
end = skipSpaces <* (string "END;" <|> string "End;") *> skipSpaces

tree :: Parser (Tree PartialNode)
tree = do
  _ <- string "Tree"
  skipSpaces
  _ <- many (letter <|> digit)
  skipSpaces
  _ <- char '='
  skipSpaces
  subTree <* char ';'
