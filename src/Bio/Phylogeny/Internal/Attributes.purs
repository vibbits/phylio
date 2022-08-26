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

attributeToString :: Attribute -> Maybe String
attributeToString (Text s) = Just s
attributeToString _ = Nothing

attributeToBool :: Attribute -> Maybe Boolean
attributeToBool (Bool b) = Just b
attributeToBool _ = Nothing

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
