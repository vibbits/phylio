module Bio.Phylogeny.Newick where

import Prelude hiding (between)

import Bio.Phylogeny.Types
  ( Attribute(..)
  , NodeType(..)
  , PNode(..)
  , Parser
  , Phylogeny
  , Tree(..)
  , interpretIntermediate
  , toPhylogeny
  )
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array as A
import Data.Either (Either)
import Data.Int as I
import Data.Interpolate (i)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Number as N
import Data.String (trim)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Parsing (fail, runParser, ParseError)
import Parsing.Combinators (between, many1, optional, optionMaybe, sepBy, try)
import Parsing.String (char, string)
import Parsing.String.Basic (oneOf, skipSpaces)
import Parsing.Token (alphaNum, digit, letter, space)

-- | Parse phylogenies serialised to the Newick format
parseNewick :: String -> Either ParseError Phylogeny
parseNewick input = runParser input newickParser

newickParser :: Parser Phylogeny
newickParser = toPhylogeny <<< interpretIntermediate 0 <$> subTree <* char ';'

subTree :: Parser (Tree PNode)
subTree = fix $ \p -> internal p <|> leaf

refp :: Parser (Tuple Int NodeType)
refp = do
  _ <- char '#'
  t <-
    (const Hybrid <$> string "H")
      <|> (const LateralGeneTransfer <$> string "LGT")
      <|> (const Recombination <$> string "R")
      <|> pure Hybrid
  n <- fromCharArray <<< A.fromFoldable <$> many1 digit
  case I.fromString n of
    Just r -> pure (r /\ t)
    Nothing -> fail $ i "References must be an integer: " n

node :: NodeType -> Parser PNode
node nt =
  let
    extras = [ '.', '_', '-' ]
  in
    do
      skipSpaces
      name' <- trim <<< fromCharArray <$> A.many (alphaNum <|> space <|> oneOf extras)
      ref <- optionMaybe refp
      optional comment
      len <- length <|> pure 0.0
      skipSpaces
      attrs <- try attributes <|> pure M.empty
      optional comment
      skipSpaces
      case ref of
        Just (id /\ nodet) -> pure $ PNode
          { name: name'
          , node: nodet
          , branchLength: len
          , attributes: attrs
          , ref: Just id
          }
        Nothing -> pure $ PNode
          { name: name'
          , node: nt
          , branchLength: len
          , attributes: attrs
          , ref: Nothing
          }

number :: Parser Number
number = do
  a <- fromCharArray <<< A.fromFoldable <$> many1 digit
  _ <- optional $ char '.'
  b <- fromCharArray <<< A.fromFoldable <$> A.many digit
  case N.fromString $ a <> "." <> b of
    Nothing -> fail "Not a number"
    Just num -> pure num

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

leaf :: Parser (Tree PNode)
leaf = Leaf <$> node Taxa

internal :: Parser (Tree PNode) -> Parser (Tree PNode)
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
