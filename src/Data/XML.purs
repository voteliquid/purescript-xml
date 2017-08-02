module Data.XML
  ( class DecodeXML
  , class EncodeXML
  , decodeXML
  , encodeXML
  , parseXML
  , module Data.XML.Types
  , module Data.XML.Combinators
  ) where

import Data.XML.Combinators (xmlChild, xmlChildren, xmlIntChild, xmlNumChild, xmlTextChild, (!?>), (#?>), (=?>), (?>), (?>>))
import Control.Alternative ((<|>))
import Data.Array (fromFoldable)
import Data.Array (singleton) as A
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(Nothing, Just))
import Data.StrMap (StrMap, empty, insert)
import Data.String (singleton)
import Data.String.Unsafe (char) as Data.String.Unsafe
import Data.Tuple (Tuple(..))
import Data.XML.Types (XML(..))
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.Combinators (lookAhead, many1Till, manyTill, try)
import Text.Parsing.Parser.String (anyChar, satisfy, skipSpaces, string)
import Prelude hiding (between)

class EncodeXML a where
  encodeXML :: a -> XML

class DecodeXML a where
  decodeXML :: XML -> Either String a

parseXML :: String -> Either ParseError XML
parseXML = flip runParser $ try (xmlDeclaration >>= \_ -> xml) <|> xml

xml :: Parser String XML
xml = do
  tagname <- openingTagName
  attrs <- tagAttributes
  let node = XMLNode tagname attrs
  (try (contentNode node)) <|> (try (selfClosingTag node)) <|> (tagWithChildren node)

xmlDeclaration :: Parser String Unit
xmlDeclaration = do
  skipSpaces
  void $ string "<?"
  void $ manyTill (singleton <$> anyChar) (string "?>")

openingTagName :: Parser String String
openingTagName = do
  skipSpaces
  void (string "<")
  foldl append "" <$> many1Till (singleton <$> anyChar) tagEnd

tagAttributes :: Parser String (StrMap String)
tagAttributes = foldl (\b (Tuple k v) -> insert k v b) empty <$> manyTill attribute tagEnd

attribute :: Parser String (Tuple String String)
attribute = do
  key <- attributeKey
  val <- attributeValue
  pure $ Tuple key val

attributeValue :: Parser String String
attributeValue = foldl append "" <$> many1Till (singleton <$> anyChar) (string "\"")

attributeKey :: Parser String String
attributeKey = foldl append "" <$> many1Till (singleton <$> anyChar) (string "=\"")

closingTag :: Parser String Unit
closingTag = do
  void $ string "</"
  skipSpaces
  void $ manyTill (singleton <$> anyChar) (skipSpaces >>= \_ -> string ">")
  skipSpaces

tagWithChildren :: (Maybe (Array XML) -> XML) -> Parser String XML
tagWithChildren f = do
  void (string ">")
  pure <<< f <<< Just <<< fromFoldable =<< many1Till xml closingTag

cdata :: Parser String String
cdata = do
  void $ string "<![CDATA["
  str <- foldl append "" <$> many1Till (singleton <$> anyChar) (string "]]>")
  closingTag
  pure str

contentNode :: (Maybe (Array XML) -> XML) -> Parser String XML
contentNode f = do
  void (string ">")
  content <- try cdata <|> (foldl append "" <$> many1Till anyButBracket closingTag)
  skipSpaces
  pure $ f $ Just $ A.singleton (XMLContent content)

anyButBracket :: Parser String String
anyButBracket = singleton <$> (satisfy \c -> Data.String.Unsafe.char "<" /= c && Data.String.Unsafe.char ">" /= c)

selfClosingTag :: ∀ a. (Maybe a -> XML) -> Parser String XML
selfClosingTag f = do
  let orphan = do
        void $ string ">"
        skipSpaces
        closingTag
  skipSpaces
  void (string "/>") <|> orphan
  skipSpaces
  pure $ f Nothing

tagEnd :: Parser String String
tagEnd = string " " <|> (lookAhead (string "/" <|> string ">"))
