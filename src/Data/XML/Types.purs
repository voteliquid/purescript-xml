module Data.XML.Types where

import Data.List (List)
import Data.Tuple (Tuple)

type XMLAttribute = Tuple XMLAttributeName XMLAttributeValue
type XMLAttributeName = String
type XMLAttributeValue = String
type XMLTagName = String

data XML
  = XMLNode XMLTagName (List XMLAttribute) (List XML)
  | XMLContent String
