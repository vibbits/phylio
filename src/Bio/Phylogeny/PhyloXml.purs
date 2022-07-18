module Bio.Phylogeny.PhyloXml where

import Prelude hiding (between)

import Bio.Phylogeny.Types
  ( Attribute(..)
  , NodeType(..)
  , PNode(..)
  , Parser
  , PartialPhylogeny(..)
  , Phylogeny
  , Tree(..)
  , Metadata
  , attributeToBool
  , attributeToString
  , interpretIntermediate
  , parseAttribute
  , toAnnotatedPhylogeny
  )
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array as A
import Data.Either (Either(..))
import Data.Filterable (filter, partitionMap)
import Data.Foldable (foldl)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, over, un)
import Data.Number as Number
import Data.String as S
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Parsing (fail, runParser, ParseError)
import Parsing.Combinators (between, choice, many1, optional, sepBy, try)
import Parsing.String (char, string)
import Parsing.String.Basic (oneOf, skipSpaces)
import Parsing.Token (alphaNum, space)

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

parsePhyloXml :: String -> Either ParseError Phylogeny
parsePhyloXml input = runParser input phyloXmlParser

phyloXmlParser :: Parser Phylogeny
phyloXmlParser = do
  tree' <- phyloxml
  case convert tree' of
    Right { meta, trees } -> do
      pure
        $ toAnnotatedPhylogeny meta
        $ foldl
            ( \acc@(PartialPhylogeny { maxRef }) p ->
                acc <> interpretIntermediate (maxRef + 1) p
            )
            mempty
            trees
    Left err -> fail err

branchLength :: M.Map String Attribute -> Maybe Number
branchLength attrs =
  case M.lookup "branch_length" attrs of
    Just (Numeric len) -> Just len
    _ -> Nothing

pnode :: NodeType -> M.Map String Attribute -> PNode
pnode nt attrs =
  PNode
    { name: fromMaybe "" $ attributeToString <$> M.lookup "name" attrs
    , node: nt
    , branchLength: fromMaybe 0.0 $ branchLength attrs
    , ref: Nothing
    , attributes: filteredAttrs
    }
  where
  filteredAttrs = M.filterKeys (\key -> A.notElem key [ "name", "branch_length" ]) attrs

metadata :: M.Map String Attribute -> Metadata
metadata attrs =
  { name: attributeToString <$> M.lookup "name" attrs
  , parent: 0
  , rooted: fromMaybe true (attributeToBool =<< M.lookup "rooted" attrs)
  , description: attributeToString <$> M.lookup "description" attrs
  }

convert :: Tree XmlNode -> Either String { meta :: Array Metadata, trees :: (Array (Tree PNode)) }
convert (Internal (XmlNode { name }) chs) =
  let
    children :: Either String (Array { meta :: Maybe Metadata, trees :: Array (Tree PNode) })
    children = sequence (convert' <$> chs)
  in
    case name of
      "phyloxml" ->
        ( \ch ->
            { meta: A.catMaybes $ _.meta <$> ch
            , trees: join $ _.trees <$> ch
            }
        ) <$> children

      _ -> Left "Not PhyloXML data: no top-level phyloxml tag"
convert (Leaf _) =
  Left "No trees in this PhyloXML document"
convert (Empty _) = Left "TODO: Remove the Empty ctor"

convert' :: Tree XmlNode -> Either String { meta :: Maybe Metadata, trees :: Array (Tree PNode) }
convert' (Internal (XmlNode xml) children) =
  let
    convertedChildren :: Array (Tree PNode)
    convertedChildren = join (_.trees <$> (partitionMap convert' children).right)
  in
    case xml.name of
      "phylogeny" ->
        Right
          { meta: Just $ metadata xml.attributes
          , trees: convertedChildren
          }

      "clade" ->
        Right
          { meta: Nothing
          , trees:
              [ Internal
                  (pnode Clade xml.attributes)
                  convertedChildren
              ]
          }
      _ -> Left $ xml.name <> " is not a known phyloXML tree node"
convert' (Leaf (XmlNode xml)) =
  Right { meta: Nothing, trees: [ Leaf $ pnode Taxa xml.attributes ] }
convert' (Empty _) = Left "TODO: Remove the Empty constructor" -- TODO:

tagNameChar :: Parser Char
tagNameChar = choice [ alphaNum, oneOf [ '.', '-', '_', ':' ] ]

tagName :: Parser String
tagName = fromCharArray <<< A.fromFoldable <$> many1 tagNameChar

reservedChars :: Parser Char
reservedChars = oneOf [ '"', '\'' ]

extraChars :: Parser Char
extraChars = oneOf
  [ '!'
  , '#'
  , '$'
  , '%'
  , '&'
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
attrValue = (between (char '"') (char '"') $ attrValue' '\'')
  <|> (between (char '\'') (char '\'') $ attrValue' '"')
  where
  attrValue' extra =
    fromCharArray <<< A.fromFoldable <$> A.many (alphaNum <|> space <|> extraChars <|> char extra)

value :: Parser String
value =
  fromCharArray <<< A.fromFoldable <$> A.many (alphaNum <|> space <|> extraChars <|> reservedChars)

attribute :: Parser (Tuple String Attribute)
attribute = do
  key <- tagName
  _ <- char '='
  val <- attrValue
  case Number.fromString val of
    Just num -> pure (key /\ Numeric num)
    _ -> pure (key /\ parseAttribute val)

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
    let
      prefixed = (un XmlNode $ prefixAttrs node'.name $ XmlNode node').attributes
    in
      XmlNode
        { name: acc.name
        , attributes:
            M.union
              (M.union prefixed acc.attributes)
              (valueAsAttr $ XmlNode node')
        , value: Nothing
        }
  mergeNode (XmlNode acc) (Internal (XmlNode node') children') =
    let
      prefixed = (un XmlNode $ prefixAttrs node'.name $ XmlNode node').attributes

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
              (M.union prefixed child.attributes)
              (M.union acc.attributes $ valueAsAttr $ XmlNode child)
        , value: Nothing
        }

  valueAsAttr :: XmlNode -> M.Map String Attribute
  valueAsAttr (XmlNode xml) =
    case parseAttribute <$> xml.value of
      Just val -> M.singleton xml.name val
      Nothing -> M.empty

  prefixAttrs :: String -> XmlNode -> XmlNode
  prefixAttrs prefix (XmlNode xml) = XmlNode (xml { attributes = go xml.attributes })
    where
    go :: M.Map String Attribute -> M.Map String Attribute
    go attrs = M.fromFoldable $ (makePrefix <$> M.toUnfoldable attrs :: Array (Tuple String Attribute))

    makePrefix :: Tuple String Attribute -> Tuple String Attribute
    makePrefix (k /\ v) = ((prefix <> "/" <> k) /\ v)
