module Data.XML.Types where

import Data.List (List)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

type XMLAttribute = Tuple XMLAttributeName XMLAttributeValue
type XMLAttributeName = String
type XMLAttributeValue = String
type XMLTagName = String

data XML
  = XMLNode XMLTagName (List XMLAttribute) (List XML)
  | XMLContent String

data XPath = LocationPath (List LocationStep)

data LocationStep = LocationStep Axis NodeTest (Maybe XPath)

data Axis = Child

data NodeTest = NameTest String | TextNodeType | AnyNodeType
