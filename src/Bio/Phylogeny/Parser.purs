module Bio.Phylogeny.Parser (Phylogeny(..), parseNewick) where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (fromFoldable, some, many)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.Interpolate (i)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as N
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Text.Parsing.Parser (ParserT, fail, runParser)
import Text.Parsing.Parser.Combinators (between, many1, optionMaybe, optional, sepBy, try)
import Text.Parsing.Parser.String (char, skipSpaces, string)
import Text.Parsing.Parser.Token (digit, letter)

--type Phylogeny = Array String
data Phylogeny = Leaf (Tuple String Number)
               | Internal (Array Phylogeny)

derive instance eqPhylogeny :: Eq Phylogeny

instance Show Phylogeny where
  show (Leaf (n /\ l)) = i "Leaf (" n ", " l ")"
  show (Internal as) = i "Internal " (show as)


type Parser a = ParserT String Identity a

parseNewick :: String -> Either String Phylogeny
parseNewick input = lmap show $ runParser input newickParser

newickParser :: Parser Phylogeny
newickParser = subTree <* char ';'

subTree :: Parser Phylogeny
subTree = fix $ \p -> internal p <|> leaf

name :: Parser (Tuple String Number)
name = do
  skipSpaces
  name' <- fromCharArray <$> many letter
  len <- length <|> pure 0.0
  skipSpaces
  pure (name' /\ len)

number :: Parser Number
number = do
  a <- fromCharArray <<< fromFoldable <$> many1 digit
  _ <- optional $ char '.'
  b <- try $ fromCharArray <<< fromFoldable <$> many1 digit
  case N.fromString $ a <> "." <> b of
    Nothing -> fail "Not a number"
    Just num -> pure num

length :: Parser Number
length = char ':' *> number

branch :: Parser Phylogeny -> Parser Phylogeny
branch pars = do
  tree <- pars
  _ <- name <|> pure ("" /\ 0.0)
  pure tree

leaf :: Parser Phylogeny
leaf = Leaf <$> name

internal :: Parser Phylogeny -> Parser Phylogeny
internal pars = do
  lst <- between (string "(") (string ")") (branch pars `sepBy` char ',')
  pure $ Internal (fromFoldable lst)
