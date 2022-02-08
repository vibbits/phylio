module Bio.Phylogeny.Nexus where

import Prelude hiding (between)

import Bio.Phylogeny.Newick (subTree)
import Bio.Phylogeny.Types
  ( PNode
  , ParseError
  , PartialPhylogeny(..)
  , Phylogeny
  , Tree
  , interpretIntermediate
  , toParseError
  , toPhylogeny
  )
import Control.Alt ((<|>))
import Data.Array (many, fromFoldable)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Filterable (filterMap)
import Data.Foldable (foldl)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.String (toUpper)
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.Parser (ParserT, fail, runParser)
import Text.Parsing.Parser.Combinators (between, lookAhead, many1, manyTill)
import Text.Parsing.Parser.String (anyChar, char, eof, skipSpaces, string)
import Text.Parsing.Parser.Token (digit, letter)

type Parser a = ParserT String Identity a

data Block = Block String (Maybe (Tree PNode))

parseNexus :: String -> Either ParseError Phylogeny
parseNexus input = lmap toParseError $ runParser input nexusParser

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

tree :: Parser (Tree PNode)
tree = do
  _ <- string "Tree"
  skipSpaces
  _ <- many (letter <|> digit)
  skipSpaces
  _ <- char '='
  skipSpaces
  subTree <* char ';'
