module Data.XML.Types where

import Data.List (List)
import Data.Map (Map)

type XMLAttributes = Map XMLAttributeName XMLAttributeValue
type XMLAttributeName = String
type XMLAttributeValue = String
type XMLTagName = String

data XML
  = XMLNode XMLTagName XMLAttributes (List XML)
  | XMLContent String
