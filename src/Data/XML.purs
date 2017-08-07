module Data.XML
  ( module Data.XML.Decode
  , module Data.XML.Encode
  , module Data.XML.Parse
  , module Data.XML.Types
  ) where

import Data.XML.Decode (class DecodeXML, decodeXML, getChild, (?>), getOptionalChild, (??>), getNestedChild, (?>>), getOptionalNestedChild, (??>>), filterChildren, (=?>))
import Data.XML.Encode (class EncodeXML, encodeXML)
import Data.XML.Parse (parseXML)
import Data.XML.Types (XML(..), XMLAttribute)
