module Data.XML.Decode
  ( class DecodeXML
  , decodeXML
  , getChild
  , getMaybeChild
  , getChildren
  , (?>)
  , (??>)
  , (?>>)
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Identity (Identity(..))
import Data.Int (fromString)
import Data.List (List(..), concat, filter, head, singleton)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Data.Traversable (sequence)
import Data.XML.Types (XML(..), Axis(..), LocationStep(..), NodeTest(..), XPath(..))
import Data.XML.XPath.Parse (parseXPath)
import Global (isFinite, readFloat)
import Text.Parsing.Parser (parseErrorMessage, runParserT)

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

getChildren :: ∀ a. DecodeXML a => XML -> String -> Either String (List a)
getChildren (XMLContent _) _ = Right Nil
getChildren ctx s = case un Identity (runParserT s parseXPath) of
  Left err -> Left (parseErrorMessage err)
  Right (LocationPath steps) -> sequence $ decodeXML <$> foldl foldXPath (singleton ctx) steps

infixl 7 getChildren as ?>>

getChild :: ∀ a. DecodeXML a => XML -> String -> Either String a
getChild (XMLContent _) s = Left $ "no node found for xpath \"" <> s <> "\""
getChild ctx s = case un Identity (runParserT s parseXPath) of
  Left err -> Left (parseErrorMessage err)
  Right (LocationPath steps) ->
    case head $ foldl foldXPath (singleton ctx) steps of
      Nothing -> Left $ "no node found for xpath \"" <> s <> "\""
      Just x -> decodeXML x

infixl 7 getChild as ?>

getMaybeChild :: ∀ a. DecodeXML a => XML -> String -> Either String (Maybe a)
getMaybeChild (XMLContent _) s = Left $ "no node found for xpath \"" <> s <> "\""
getMaybeChild ctx s = case un Identity (runParserT s parseXPath) of
  Left err -> Left (parseErrorMessage err)
  Right (LocationPath steps) ->
    case head $ foldl foldXPath (singleton ctx) steps of
      Nothing -> Right Nothing
      Just x -> Just <$> decodeXML x

infixl 7 getMaybeChild as ??>

foldXPath :: List XML -> LocationStep -> List XML
foldXPath xs step = concat $ map (\x -> select x step) xs

select :: XML -> LocationStep -> List XML
select (XMLContent _) _ = Nil
select ctx (LocationStep Child AnyNodeType pred) = childNodes ctx
select ctx (LocationStep Child TextNodeType pred) = filter filterTextNode (childNodes ctx)
select ctx (LocationStep Child (NameTest n) pred) = filter (filterNodeName n) (childNodes ctx)

filterNodeName :: String -> XML -> Boolean
filterNodeName name (XMLContent _) = false
filterNodeName name (XMLNode n _ _) = n == name

filterTextNode :: XML -> Boolean
filterTextNode (XMLContent _) = true
filterTextNode (XMLNode _ _ _) = false

childNodes :: XML -> List XML
childNodes (XMLContent _) = Nil
childNodes (XMLNode _ _ kids) = kids
