module Bio.Phylogeny.PhyloXml where

import Prelude hiding (between)

import Bio.Phylogeny.Types (Attribute(..), NodeType(..), PNode(..), Parser, Phylogeny, Tree(..), interpretIntermediate)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Debug (trace)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.Combinators (between, choice, lookAhead, many1, sepBy, try)
import Text.Parsing.Parser.String (char, oneOf, skipSpaces, string)
import Text.Parsing.Parser.Token (alphaNum, space)

type XmlNode =
  { name :: String
  , attributes :: M.Map String Attribute
  , value :: Maybe String
  }

parsePhyloXml :: String -> Either String (Tree XmlNode)
parsePhyloXml input = lmap show $ runParser input phyloxml -- phyloXmlParser

phyloXmlParser :: Parser Phylogeny
phyloXmlParser = interpretIntermediate <$> (pure $ Leaf pnode)
  where
  pnode :: PNode
  pnode = PNode
    { name: "test"
    , node: Clade
    , branchLength: 0.0
    , ref: Nothing
    , attributes: M.empty
    }

nameChar :: Parser Char
nameChar = choice [ alphaNum, oneOf [ '.', '-', '_', ':' ] ]

name :: Parser String
name = fromCharArray <<< A.fromFoldable <$> many1 nameChar

extraChars :: Parser Char
extraChars = oneOf
  [ '!'
  , '#'
  , '$'
  , '%'
  , '&'
  , '\''
  , '('
  , ')'
  , '*'
  , '+'
  , ','
  , '-'
  , '.'
  , '/'
  , ':'
  , ';'
  , '='
  , '?'
  , '@'
  , '['
  , '\\'
  , ']'
  , '^'
  , '_'
  , '`'
  , '{'
  , '|'
  , '}'
  , '~'
  , '¡'
  , '¢'
  , '£'
  , '¤'
  , '¥'
  , '¦'
  , '§'
  , '¨'
  , '©'
  , 'ª'
  , '«'
  , '¬'
  , '®'
  , '¯'
  , '°'
  , '±'
  , '²'
  , '³'
  , '´'
  , 'µ'
  , '¶'
  , '·'
  , '¸'
  , '¹'
  , 'º'
  , '»'
  , '¼'
  , '½'
  , '¾'
  , '¿'
  , 'À'
  , 'Á'
  , 'Â'
  , 'Ã'
  , 'Ä'
  , 'Å'
  , 'Æ'
  , 'Ç'
  , 'È'
  , 'É'
  , 'Ê'
  , 'Ë'
  , 'Ì'
  , 'Í'
  , 'Î'
  , 'Ï'
  , 'Ð'
  , 'Ñ'
  , 'Ò'
  , 'Ó'
  , 'Ô'
  , 'Õ'
  , 'Ö'
  , '×'
  , 'Ø'
  , 'Ù'
  , 'Ú'
  , 'Û'
  , 'Ü'
  , 'Ý'
  , 'Þ'
  , 'ß'
  , 'à'
  , 'á'
  , 'â'
  , 'ã'
  , 'ä'
  , 'å'
  , 'æ'
  , 'ç'
  , 'è'
  , 'é'
  , 'ê'
  , 'ë'
  , 'ì'
  , 'í'
  , 'î'
  , 'ï'
  , 'ð'
  , 'ñ'
  , 'ò'
  , 'ó'
  , 'ô'
  , 'õ'
  , 'ö'
  , '÷'
  , 'ø'
  , 'ù'
  , 'ú'
  , 'û'
  , 'ü'
  , 'ý'
  , 'þ'
  , 'ÿ'
  ]

value :: Parser String
value =
  fromCharArray <<< A.fromFoldable <$> A.many (alphaNum <|> space <|> extraChars)

attribute :: Parser (Tuple String Attribute)
attribute = do
  key <- name
  _ <- char '='
  val <- between (char '"') (char '"') value
  pure (key /\ Text val)

open :: Parser (Tuple String (M.Map String Attribute))
open = do
  skipSpaces
  _ <- char '<'
  skipSpaces
  tag <- name
  skipSpaces
  attrs <- A.fromFoldable <$> attribute `sepBy` (char ' ')
  skipSpaces
  _ <- char '>'
  pure $ (tag /\ M.fromFoldable attrs)

close :: String -> Parser Unit
close tag = string "</" *> skipSpaces *> string tag *> skipSpaces *> string ">" *> pure unit

element :: Parser (Tree XmlNode) -> Parser (Tree XmlNode)
element inner = choice
  [ try emptyElementTag
  , sTag inner
  ]

sTag :: Parser (Tree XmlNode) -> Parser (Tree XmlNode)
sTag inner = do
  (tag /\ attrs) <- open
  subTree <- A.many (try inner)
  val <- try value
  _ <- close tag
  let node = \theVal -> { name: tag, attributes: attrs, value: theVal }
  if A.null subTree then
    pure $ Leaf $ node $ Just val
  else
    pure $ Internal (node Nothing) subTree

emptyElementTag :: Parser (Tree XmlNode)
emptyElementTag = do
  skipSpaces
  _ <- char '<'
  skipSpaces
  tag <- name
  skipSpaces
  attrs <- A.many attribute
  skipSpaces
  _ <- string "/>"
  pure $ Leaf { name: tag, attributes: M.fromFoldable attrs, value: Nothing }

--(string $ "<" <> tag) *> skipSpaces *> inner <* string "/>"

-- phyloxml :: Parser (Tree PNode)
-- phyloxml = element

phyloxml :: Parser (Tree XmlNode)
phyloxml = fix element
