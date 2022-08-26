module Bio.Phylogeny.Internal.Newick (parseNewick, subTree) where

import Prelude hiding (between)

import Bio.Phylogeny.Internal.Attributes (Attribute(..))
import Bio.Phylogeny.Internal.Types
  ( Event(..)
  , Parser
  , PartialNode
  , Phylogeny
  , Tree(..)
  , interpretIntermediate
  , toPhylogeny
  )
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array as A
import Data.Either (Either)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Number as N
import Data.String (trim)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Parsing (ParseError, runParser)
import Parsing.Combinators (between, many1, optionMaybe, sepBy, try)
import Parsing.String (char, string)
import Parsing.String.Basic (intDecimal, number, oneOf, skipSpaces)
import Parsing.Token (alphaNum, letter, space)

-- | Parse phylogenies serialised to the Newick format
parseNewick :: String -> Either ParseError Phylogeny
parseNewick input = runParser input newickParser

newickParser :: Parser Phylogeny
newickParser = toPhylogeny <<< interpretIntermediate 0 <$> subTree <* char ';'

subTree :: Parser (Tree PartialNode)
subTree = fix $ \p -> internal p <|> leaf

refp :: Parser (Tuple Int Event)
refp = do
  _ <- char '#'
  t <-
    (const Hybrid <$> string "H")
      <|> (const LateralGeneTransfer <$> string "LGT")
      <|> (const Recombination <$> string "R")
      <|> pure Hybrid
  n <- intDecimal
  pure (n /\ t)

node :: Event -> Parser PartialNode
node event =
  let
    extras = [ '.', '_', '-' ]
  in
    do
      skipSpaces
      name' <- trim <<< fromCharArray <$> A.many (alphaNum <|> space <|> oneOf extras)
      ref <- optionMaybe refp
      try comment <|> pure unit
      len <- length <|> pure 0.0
      skipSpaces
      attrs <- try attributes <|> pure M.empty
      try comment <|> pure unit
      skipSpaces
      case ref of
        Just (id /\ event') -> pure
          { name: name'
          , event: event'
          , branchLength: len
          , attributes: attrs
          , ref: Just id
          }
        Nothing -> pure
          { name: name'
          , event: event
          , branchLength: len
          , attributes: attrs
          , ref: Nothing
          }

length :: Parser Number
length = char ':' *> number

attributes :: Parser (M.Map String Attribute)
attributes = do
  kvs <- between (string "[&&NHX:") (string "]") (attr `sepBy` char ':')
  pure $ M.fromFoldable kvs

attr :: Parser (Tuple String Attribute)
attr = do
  key <- fromCharArray <<< A.fromFoldable <$> many1 letter
  _ <- char '='
  val <- fromCharArray <<< A.fromFoldable <$> many1 (alphaNum <|> char '.')
  case N.fromString val of
    Just value -> pure (key /\ Numeric value)
    Nothing -> pure (key /\ Text val)

leaf :: Parser (Tree PartialNode)
leaf = Leaf <$> node Taxa

internal :: Parser (Tree PartialNode) -> Parser (Tree PartialNode)
internal pars = do
  lst <- between (string "(") (string ")") ((skipSpaces *> pars <* skipSpaces) `sepBy` char ',')
  parent <- try $ node Clade
  pure $ Internal parent $ A.fromFoldable lst

comment :: Parser Unit
comment =
  between (char '[') (char ']') $
    A.many
      ( alphaNum <|>
          ( oneOf
              [ '&'
              , '='
              , '.'
              , '_'
              , '-'
              , '+'
              ]
          )
      ) *> pure unit
