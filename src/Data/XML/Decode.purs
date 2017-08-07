module Data.XML.Decode where

import Prelude
import Data.Array (snoc) as A
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.List (List(..), filter, head, snoc)
import Data.Maybe (Maybe(..), maybe)
import Data.XML.Types (XML(..), XMLTagName)
import Global (isFinite, readFloat)

class DecodeXML a where
  decodeXML :: XML -> Either String a

instance decodeXmlXml :: DecodeXML XML where
  decodeXML = Right

instance decodeXmlString :: DecodeXML String where
  decodeXML (XMLContent a) = Right a
  decodeXML (XMLNode _ _ kids) = maybe (Right "") decodeXML (head kids)

instance decodeXmlNumber :: DecodeXML Number where
  decodeXML (XMLContent a) =
    let n = readFloat a
    in if isFinite n then Right n else Left "expected XML content to be number"
  decodeXML (XMLNode _ _ kids) = maybe (Right 0.0) decodeXML (head kids)

instance decodeXmlInt :: DecodeXML Int where
  decodeXML (XMLContent a) = maybe (Left "expected XML content to be integer") Right (fromString a)
  decodeXML (XMLNode _ _ kids) = maybe (Right 0) decodeXML (head kids)

instance decodeXmlUnit :: DecodeXML Unit where
  decodeXML _ = Right unit

instance decodeXmlMaybe :: DecodeXML a => DecodeXML (Maybe a) where
  decodeXML (XMLNode _ _ kids) = maybe (Right Nothing) decodeXML (head kids)
  decodeXML content = do
    a <- decodeXML content
    pure (Just a)

instance decodeXmlList :: DecodeXML a => DecodeXML (List a) where
  decodeXML (XMLNode _ _ kids) =
    let fold (Left err) _ = Left err
        fold (Right as) x = do
          a <- decodeXML x
          pure (snoc as a)
     in foldl fold (Right Nil) kids
  decodeXML _ = Left "expected XML node"

instance decodeXmlArray :: DecodeXML a => DecodeXML (Array a) where
  decodeXML (XMLNode _ _ kids) =
    let fold (Left err) _ = Left err
        fold (Right as) x = do
          a <- decodeXML x
          pure (A.snoc as a)
     in foldl fold (Right []) kids
  decodeXML _ = Left "expected XML node"

getChild :: ∀ a. DecodeXML a => XML -> XMLTagName -> Either String a
getChild (XMLContent _) tag = Left $ "expected an xml node but found content node"
getChild (XMLNode n _ kids) tag =
  let fold b node@(XMLNode name _ _) = if name == tag then decodeXML node else b
      fold b node@(XMLContent _) = b
   in foldl fold (Left ("no node found with tag \"" <> tag <> "\"")) kids

infixl 7 getChild as ?>

getOptionalChild :: ∀ a. DecodeXML a => XML -> XMLTagName -> Either String (Maybe a)
getOptionalChild (XMLContent _) tag = Left $ "expected an xml node but found content node"
getOptionalChild (XMLNode n _ kids) tag =
  let fold b node@(XMLNode name _ _) = if name == tag then decodeXML node else b
      fold b node@(XMLContent _) = b
   in foldl fold (Right Nothing) kids

infixl 7 getOptionalChild as ??>

getOptionalNestedChild :: ∀ a. DecodeXML a => Either String (Maybe XML) -> XMLTagName -> Either String (Maybe a)
getOptionalNestedChild (Left err) tag = Left err
getOptionalNestedChild (Right Nothing) tag = Right Nothing
getOptionalNestedChild (Right (Just (XMLContent _))) tag = Right Nothing
getOptionalNestedChild (Right (Just (XMLNode n _ kids))) tag =
  let fold b node@(XMLNode name _ _) = if name == tag then decodeXML node else b
      fold b node@(XMLContent _) = b
   in foldl fold (Right Nothing) kids

infixl 7 getOptionalNestedChild as ??>>

getNestedChild :: ∀ a. DecodeXML a => Either String XML -> XMLTagName -> Either String a
getNestedChild (Left err) _ = Left err
getNestedChild (Right x) tag = getChild x tag

infixl 6 getNestedChild as ?>>

-- Return XML node with children filtered by tag name.
filterChildren :: ∀ a. DecodeXML a => Either String XML -> XMLTagName -> Either String a
filterChildren (Left err) _ = Left err
filterChildren (Right (XMLNode n a kids)) tag = decodeXML $ XMLNode n a (filter eqTag kids)
  where
  eqTag (XMLNode name _ _) = name == tag
  eqTag (XMLContent _) = false
filterChildren (Right (XMLContent _)) _ = Left "expected an xml node but found content node"

infixl 5 filterChildren as =?>
