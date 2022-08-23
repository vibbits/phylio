module Bio.Phylogeny.PhyloXml where

import Prelude hiding (between)

import Bio.Phylogeny.Types
  ( Attribute(..)
  , Metadata
  , NodeType(..)
  , PNode(..)
  , Parser
  , PartialPhylogeny(..)
  , Phylogeny
  , Tree(..)
  , attributeToBool
  , attributeToString
  , interpretIntermediate
  , parseAttribute
  , toAnnotatedPhylogeny
  )
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array as A
import Data.Array.NonEmpty as NA
import Data.Either (Either(..))
import Data.Filterable (partitionMap)
import Data.Foldable (foldl)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Number as Number
import Data.Number.Format (toString)
import Data.String as S
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Parsing (fail, runParser, ParseError)
import Parsing.Combinators (between, choice, many1, optional, sepEndBy, try)
import Parsing.String (anyTill, char, string)
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
    { name: fromMaybe "" (coerceName <$> M.lookup "name" attrs)
    , node: nt
    , branchLength: fromMaybe 0.0 $ branchLength attrs
    , ref: Nothing
    , attributes: filteredAttrs
    }
  where
  filteredAttrs = M.filterKeys (\key -> A.notElem key [ "name", "branch_length" ]) attrs

  coerceName :: Attribute -> String
  coerceName (Text t) = t
  coerceName (Numeric n) = toString n
  coerceName (Bool b) = show b
  coerceName (List _) = "List ..."
  coerceName (Mapping _) = "Mapping ..."

metadata :: M.Map String Attribute -> Metadata
metadata attrs =
  { name: M.lookup "name" attrs >>= attributeToString
  , parent: 0
  , rooted: fromMaybe true (attributeToBool =<< M.lookup "rooted" attrs)
  , description: M.lookup "description" attrs >>= attributeToString
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

tagNameChar :: Parser Char
tagNameChar = choice [ alphaNum, oneOf [ '.', '-', '_', ':', '!' ] ]

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
attributes = M.fromFoldable <$> attribute `sepEndBy` (many1 space)

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
  if (S.take 3 tag) == "!--" then do -- this is a comment
    (comment /\ _) <- anyTill (string "-->")
    pure $ Leaf
      ( XmlNode
          { name: "comment"
          , attributes: M.empty
          , value: Just (S.drop 3 tag <> comment)
          }
      )
  else do
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
  pure $ toStructuralTree tree

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

-- TODO: This feels like it could/should be a fold
toStructuralTree :: Tree XmlNode -> Tree XmlNode
toStructuralTree leaf@(Leaf _) = leaf
toStructuralTree (Internal p@(XmlNode node) children) =
  if A.null structuralElements.yes then
    Leaf (collapseNode structuralElements.no)
  else
    Internal (collapseNode structuralElements.no)
      $ toStructuralTree <$> structuralElements.yes
  where
  structuralElements
    :: { no :: Array (Tree XmlNode)
       , yes :: Array (Tree XmlNode)
       }
  structuralElements = A.partition isStructuralElement children

  -- Structural elements form the phylogeny,
  -- all other elements are attributes.
  isStructuralElement :: Tree XmlNode -> Boolean
  isStructuralElement (Leaf _) = false
  isStructuralElement (Internal (XmlNode { name }) _) =
    A.elem name [ "phyloxml", "phylogeny", "clade" ]

  -- `node` _is_ a structural element. children _are not_ all the way down.
  collapseNode :: Array (Tree XmlNode) -> XmlNode
  collapseNode chs =
    XmlNode
      { name: node.name
      , value: node.value
      , attributes: collapseTree p chs
      }

collapseAttrs :: Array (Tree XmlNode) -> Attribute
collapseAttrs cs =
  case cs of
    [ single ] ->
      collapseAttr single
    _ ->
      List (collapseAttr <$> cs)

collapseAttr :: Tree XmlNode -> Attribute
collapseAttr (Leaf (XmlNode n)) =
  case n.value of
    Nothing ->
      Mapping n.attributes
    Just val ->
      parseAttribute val

collapseAttr (Internal parent cs) =
  Mapping $ collapseTree parent cs

nodeName :: Tree XmlNode -> String
nodeName (Leaf (XmlNode n)) = n.name
nodeName (Internal (XmlNode n) _) = n.name

collapseName :: NA.NonEmptyArray (Tree XmlNode) -> String
collapseName = nodeName <<< NA.head

mkMapping :: NA.NonEmptyArray (Tree XmlNode) -> Tuple String Attribute
mkMapping attrs =
  (collapseName attrs /\ (collapseAttrs $ NA.toArray attrs))

collapseTree :: XmlNode -> Array (Tree XmlNode) -> M.Map String Attribute
collapseTree (XmlNode parent) children =
  M.union parent.attributes
    $ M.fromFoldable
        (mkMapping <$> A.groupAllBy (comparing nodeName) children')
  where
  children' :: Array (Tree XmlNode)
  children' =
    A.filter (\tree -> nodeName tree /= "comment") children
