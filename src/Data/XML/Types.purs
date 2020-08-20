module Data.XML.Types where

import Prelude (class Show, class Eq)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
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

derive instance genericXML :: Generic XML _

instance showXML :: Show XML where 
  show xml = genericShow xml

instance eqXML :: Eq XML where 
  eq xml = genericEq xml

data XPath = LocationPath (List LocationStep)

data LocationStep = LocationStep Axis NodeTest (Maybe XPath)

data Axis = Child

data NodeTest = NameTest String | TextNodeType | AnyNodeType
