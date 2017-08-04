module Data.XML.Encode where

import Data.XML.Types (XML)

class EncodeXML a where
  encodeXML :: a -> XML
