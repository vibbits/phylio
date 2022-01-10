module Bio.Phylogeny.PhyloXml where

import Prelude hiding (between)

import Bio.Phylogeny.Types (Attribute(..), NodeType(..), PNode(..), Parser, Phylogeny, Tree(..), interpretIntermediate)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.Monad.State (State, get, put, modify)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Filterable (filter, filterMap)
import Data.Foldable (foldl)
import Data.Interpolate (i)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, over, un)
import Data.Number as Number
import Data.String as S
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Debug (trace)
import Text.Parsing.Parser (fail, runParser)
import Text.Parsing.Parser.Combinators (between, choice, many1, optional, sepBy, try)
import Text.Parsing.Parser.String (char, oneOf, skipSpaces, string)
import Text.Parsing.Parser.Token (alphaNum, letter, space)

newtype XmlNode = XmlNode
  { name :: String
  , attributes :: M.Map String Attribute
  , value :: Maybe String
  }

derive instance newtypeXmlNode :: Newtype XmlNode _

derive instance eqXmlNode :: Eq XmlNode

instance showXmlNode :: Show XmlNode where
  show (XmlNode node) =
    "XmlNode{" <> show node <> "}"

parsePhyloXml :: String -> Either String Phylogeny
parsePhyloXml input = lmap show $ runParser input phyloXmlParser'

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

phyloXmlParser' :: Parser Phylogeny
phyloXmlParser' = do
  tree' <- phyloxml
  if isPhyloXml tree' then
    maybe (fail "Structure does not match") (\tree -> interpretIntermediate <$> pure tree) $ convert tree'
  else
    fail "Not a PhyloXML document"

isPhyloXml :: Tree XmlNode -> Boolean
isPhyloXml (Internal (XmlNode { name: "phyloxml" }) _) = true
isPhyloXml _ = false

branchLength :: M.Map String Attribute -> Maybe Number
branchLength attrs =
  case M.lookup "branch_length" attrs of
    Just (Numeric len) -> Just len
    _ -> Nothing

pnode :: M.Map String Attribute -> PNode
pnode attrs =
  PNode
    { name: fromMaybe "" $ show <$> M.lookup "name" attrs
    , node: Clade
    , branchLength: fromMaybe 0.0 $ branchLength attrs
    , ref: Nothing
    , attributes: filteredAttrs
    }
  where
  filteredAttrs = M.filterKeys (\key -> A.notElem key [ "name", "branch_length" ]) attrs

convert :: Tree XmlNode -> Maybe (Tree PNode)
convert (Internal (XmlNode xml@{ name }) chs) =
  if A.elem name [ "phyloxml", "phylogeny" ] then
    case A.head chs of
      Just phylogeny -> convert phylogeny
      _ -> Nothing
  else if name == "clade" then
    Just
      ( Internal
          (pnode xml.attributes)
          (filterMap convert chs)
      )
  else
    Nothing

convert (Leaf (XmlNode xml)) =
  pure $ Leaf $ pnode xml.attributes

convert (Empty empty) = Just $ Empty empty

tagNameChar :: Parser Char
tagNameChar = choice [ alphaNum, oneOf [ '.', '-', '_', ':' ] ]

tagName :: Parser String
tagName = fromCharArray <<< A.fromFoldable <$> many1 tagNameChar

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
  key <- tagName
  _ <- char '='
  val <- between (char '"') (char '"') attrValue
  case Number.fromString val of
    Just num -> pure (key /\ Numeric num)
    _ -> pure (key /\ Text val)

attributes :: Parser (M.Map String Attribute)
attributes = M.fromFoldable <$> attribute `sepBy` (many1 space)

open :: Parser (Tuple String (M.Map String Attribute))
open = do
  skipSpaces
  tag <- try (char '<' *> skipSpaces *> tagName)
  skipSpaces
  attrs <- attributes
  skipSpaces
  _ <- char '>'
  pure $ (tag /\ attrs)

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
  subTree <- A.many inner
  val <- try value
  skipSpaces
  _ <- close tag
  skipSpaces
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
  tag <- tagName
  skipSpaces
  attrs <- attributes
  skipSpaces
  _ <- string "/>"
  skipSpaces
  pure $ Leaf (XmlNode { name: tag, attributes: attrs, value: Nothing })

header :: Parser Unit
header = do
  _ <- string "<?xml"
  skipSpaces
  _ <- attributes
  _ <- string "?>"
  pure unit

phyloxml :: Parser (Tree XmlNode)
phyloxml = do
  optional header
  tree <- fix element
  pure $ mergeAttributes tree

fromTextAttribute :: String -> M.Map String Attribute -> String
fromTextAttribute key attrs =
  case M.lookup key attrs of
    Just (Text val) -> val
    _ -> ""

fromNumericAttribute :: String -> M.Map String Attribute -> Number
fromNumericAttribute key attrs =
  case M.lookup key attrs of
    Just (Numeric val) -> val
    _ -> 0.0

mergeAttributes :: Tree XmlNode -> Tree XmlNode
mergeAttributes leaf@(Leaf _) = leaf
mergeAttributes e@(Empty _) = e
mergeAttributes (Internal node children) =
  if A.null mergedChildren then
    Leaf mergedNode
  else
    Internal mergedNode mergedChildren
  where
  mergedNode :: XmlNode
  mergedNode = foldl mergeNode node $ filter (not <<< isTreeElement) children

  mergedChildren :: Array (Tree XmlNode)
  mergedChildren = mergeAttributes <$> filter isTreeElement children

  isTreeElement :: Tree XmlNode -> Boolean
  isTreeElement (Leaf _) = false
  isTreeElement (Empty _) = false
  isTreeElement (Internal (XmlNode { name }) _) = A.elem name [ "phyloxml", "phylogeny", "clade" ]

  mergeNode :: XmlNode -> Tree XmlNode -> XmlNode
  mergeNode acc (Empty attrs) =
    over XmlNode (\node' -> node' { attributes = M.union node'.attributes attrs }) acc
  mergeNode (XmlNode acc) (Leaf (XmlNode node')) =
    XmlNode
      { name: acc.name
      , attributes:
          M.union
            (M.union node'.attributes acc.attributes)
            (valueAsAttr $ XmlNode node')
      , value: Nothing
      }
  mergeNode (XmlNode acc) (Internal (XmlNode node') children') =
    let
      (XmlNode child) = prefixAttrs node'.name $
        foldl mergeNode
          ( XmlNode
              { name: "", attributes: M.empty, value: Nothing }
          )
          children'
    in
      XmlNode
        { name: acc.name
        , attributes:
            M.union
              (M.union node'.attributes child.attributes)
              (M.union acc.attributes $ valueAsAttr $ XmlNode child)
        , value: Nothing
        }

  valueAsAttr :: XmlNode -> M.Map String Attribute
  valueAsAttr (XmlNode xml) =
    case xml.value of
      Just val -> M.singleton xml.name $ Text val
      Nothing -> M.empty

  prefixAttrs :: String -> XmlNode -> XmlNode
  prefixAttrs prefix (XmlNode xml) =
    XmlNode
      { name: xml.name
      , attributes: go xml.attributes
      , value: xml.value
      }
    where
    go :: M.Map String Attribute -> M.Map String Attribute
    go attrs = M.fromFoldable $ (makePrefix <$> M.toUnfoldable attrs :: Array (Tuple String Attribute))

    makePrefix :: Tuple String Attribute -> Tuple String Attribute
    makePrefix (k /\ v) = ((prefix <> "/" <> k) /\ v)
