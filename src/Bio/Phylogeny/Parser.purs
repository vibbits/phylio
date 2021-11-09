module Bio.Phylogeny.Parser (Phylogeny, parseNewick) where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Data.Array (fromFoldable, some)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Number as N
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.Parser (ParserT, fail, runParser)
import Text.Parsing.Parser.Combinators (between, many1, optionMaybe, optional, sepBy, try)
import Text.Parsing.Parser.String (char, skipSpaces, string)
import Text.Parsing.Parser.Token (digit, letter)

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
  _ <- optionMaybe withBranchLength
  _ <- skipSpaces
  pure name'

number :: Parser Number
number = do
  a <- fromCharArray <<< fromFoldable <$> many1 digit
  _ <- optional $ char '.'
  b <- try $ fromCharArray <<< fromFoldable <$> many1 digit
  case N.fromString $ a <> "." <> b of
    Nothing -> fail "Not a number"
    Just num -> pure num

withBranchLength :: Parser Number
withBranchLength = do
  _ <- char ':'
  num <- number
  pure num

singleNode :: Parser Phylogeny
singleNode =
  pure <$> name

treeNodes :: Parser Phylogeny
treeNodes = do
  lst <- between (string "(") (string ")") (name `sepBy` char ',')
  pure $ fromFoldable lst
