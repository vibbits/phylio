module Bio.Phylogeny.Parser (Phylogeny, parseNewick) where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Data.Array (fromFoldable, some)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.Parser (ParserT, runParser)
import Text.Parsing.Parser.Combinators (between, sepBy)
import Text.Parsing.Parser.String (char, skipSpaces, string)
import Text.Parsing.Parser.Token (letter)

type Phylogeny = Array String

type Parser a = ParserT String Identity a

parseNewick :: String -> Either String Phylogeny
parseNewick input = lmap show $ runParser input newickParser

newickParser :: Parser Phylogeny
newickParser = do
  tree <- treeNodes <|> singleNode
  _ <- char ';'
  pure tree

name :: Parser String
name = do
  _ <- skipSpaces
  name' <- fromCharArray <$> some letter
  _ <- skipSpaces
  pure name'

singleNode :: Parser Phylogeny
singleNode =
  pure <$> name

treeNodes :: Parser Phylogeny
treeNodes = do
  lst <- between (string "(") (string ")") (name `sepBy` char ',')
  pure $ fromFoldable lst
