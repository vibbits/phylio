module Bio.Phylogeny.PhyloXml where

import Prelude hiding (between)

import Bio.Phylogeny.Newick (attr)
import Bio.Phylogeny.Types (Attribute(..), NodeType(..), PNode(..), Parser, Phylogeny, Tree(..), interpretIntermediate)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Interpolate (i)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String as S
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Debug (trace)
import Text.Parsing.Parser (runParser, position)
import Text.Parsing.Parser.Combinators (between, choice, lookAhead, many1, optional, sepBy, try)
import Text.Parsing.Parser.String (char, oneOf, skipSpaces, string)
import Text.Parsing.Parser.Token (alphaNum, space)

newtype XmlNode = XmlNode
  { name :: String
  , attributes :: M.Map String Attribute
  , value :: Maybe String
  }

derive instance newtypeXmlNode :: Newtype XmlNode _

derive instance eqXmlNode :: Eq XmlNode

instance showXmlNode :: Show XmlNode where
  show (XmlNode node) =
    i "XmlNode{" (show node) "}"

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

reservedChars :: Parser Char
reservedChars = oneOf [ '"' ]

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

attrValue :: Parser String
attrValue =
  fromCharArray <<< A.fromFoldable <$> A.many (alphaNum <|> space <|> extraChars)

value :: Parser String
value =
  fromCharArray <<< A.fromFoldable <$> A.many (alphaNum <|> space <|> extraChars <|> reservedChars)

attribute :: Parser (Tuple String Attribute)
attribute = do
  key <- name
  _ <- char '='
  val <- between (char '"') (char '"') attrValue
  pure (key /\ Text val)

open :: Parser (Tuple String (M.Map String Attribute))
open = do
  skipSpaces
  -- _ <- char '<'
  -- skipSpaces
  tag <- try (char '<' *> skipSpaces *> name)
  skipSpaces
  attrs <- A.fromFoldable <$> attribute `sepBy` (many1 space)
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
  skipSpaces
  (tag /\ attrs) <- open
  skipSpaces
  posOpen <- position
  let _ = trace ("pos open tag: " <> show posOpen) identity
  subTree <- A.many inner
  val <- try value
  let _ = trace ("subTree: " <> (show subTree) <> "  (" <> show val <> ")") identity
  skipSpaces
  _ <- close tag
  skipSpaces
  posClose <- position
  let _ = trace ("pos close tag: " <> show posClose) identity
  let node = \theVal -> XmlNode { name: tag, attributes: attrs, value: theVal }
  if A.null subTree then
    pure $ Leaf $ node $ if S.null val then Nothing else Just val
  else
    pure $ Internal (node Nothing) subTree

emptyElementTag :: Parser (Tree XmlNode)
emptyElementTag = do
  skipSpaces
  _ <- char '<'
  skipSpaces
  tag <- name
  skipSpaces
  attrs <- A.fromFoldable <$> attribute `sepBy` (many1 space)
  skipSpaces
  _ <- string "/>"
  skipSpaces
  pure $ Leaf (XmlNode { name: tag, attributes: M.fromFoldable attrs, value: Nothing })

--(string $ "<" <> tag) *> skipSpaces *> inner <* string "/>"

-- phyloxml :: Parser (Tree PNode)
-- phyloxml = element

header :: Parser Unit
header = do
  _ <- string "<?xml"
  skipSpaces
  _ <- attribute `sepBy` (many1 space)
  _ <- string "?>"
  pure unit

phyloxml :: Parser (Tree XmlNode)
phyloxml = do
  optional header
  fix element
