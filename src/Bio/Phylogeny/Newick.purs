module Bio.Phylogeny.Newick where

import Bio.Phylogeny.Types (Attribute(..), NodeType(..), PNode(..), Parser, Phylogeny, Tree(..), interpretIntermediate)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Int as I
import Data.Interpolate (i)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Number as N
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Prelude (bind, const, discard, pure, show, ($), (*>), (<$>), (<*), (<<<), (<>))
import Text.Parsing.Parser (fail, runParser)
import Text.Parsing.Parser.Combinators (between, many1, optional, optionMaybe, sepBy, try)
import Text.Parsing.Parser.String (char, oneOf, skipSpaces, string)
import Text.Parsing.Parser.Token (digit, letter)

-- | Parse phylogenies serialised to the Newick format
parseNewick :: String -> Either String Phylogeny
parseNewick input = lmap show $ runParser input newickParser

newickParser :: Parser Phylogeny
newickParser = interpretIntermediate <$> subTree <* char ';'

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
    extras = [ '.', '_' ]
  in
    do
      skipSpaces
      name' <- fromCharArray <$> A.many (letter <|> digit <|> oneOf extras)
      ref <- optionMaybe refp
      len <- length <|> pure 0.0
      skipSpaces
      attrs <- attributes <|> pure M.empty
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
  val <- fromCharArray <<< A.fromFoldable <$> many1 (letter <|> digit <|> oneOf [ '.' ])
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

