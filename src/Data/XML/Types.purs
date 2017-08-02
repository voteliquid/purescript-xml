module Data.XML.Types where

import Data.Maybe (Maybe)
import Data.StrMap (StrMap)

data XML
  = XMLNode String (StrMap String) (Maybe (Array XML))
  | XMLContent String
