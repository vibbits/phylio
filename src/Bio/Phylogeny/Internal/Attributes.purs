module Bio.Phylogeny.Internal.Attributes
  ( Attribute(..)
  , attributeToString
  , attributeToBool
  , parseAttribute
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Data.String.Regex as RE
import Data.String.Regex.Flags (noFlags)

-- | An `Attribute` describes some information about a node in the graph.
-- | This type represents the structure for storing attributes, _not_
-- | what attributes can be stored.
data Attribute
  = Numeric Number
  | Text String
  | Bool Boolean
  | List (Array Attribute)
  | Mapping (Map String Attribute)

derive instance eqAttribute :: Eq Attribute

instance showAttribute :: Show Attribute where
  show (Numeric n) = show n
  show (Text t) = t
  show (Bool b) = show b
  show (List l) = show l
  show (Mapping m) = show m

-- | Convert a `Text` attribute to a string, otherwise Nothing
-- |
-- | ```purescript run
-- | > attributeToString (Text "hello")
-- | Just "hello"
-- |
-- | > attributeToString (Bool true)
-- | Nothing
-- | ```
attributeToString :: Attribute -> Maybe String
attributeToString (Text s) = Just s
attributeToString _ = Nothing

-- | Convert a `Text` attribute to a string, otherwise Nothing
-- |
-- | ```purescript run
-- | > attributeToBool (Text "hello")
-- | Nothing
-- |
-- | > attributeToBool (Bool true)
-- | Just true
-- | ```
attributeToBool :: Attribute -> Maybe Boolean
attributeToBool (Bool b) = Just b
attributeToBool _ = Nothing

-- | Convert an arbitrary string into an Attribute
parseAttribute :: String -> Attribute
parseAttribute attr =
  -- If the string contains only numeric characters try to make a number
  -- This is because `fromString` will accept string that onlt *start* with numbers
  -- so a value like "25_BRACA" will become `Numeric 25` otherwise
  if (flip RE.test attr <$> RE.regex "^\\d*.?\\d*$" noFlags) == Right true then
    case fromString attr of
      Just num -> Numeric num
      _ -> Text attr
  else
    case attr of
      "true" -> Bool true
      "false" -> Bool false
      _ -> Text attr
