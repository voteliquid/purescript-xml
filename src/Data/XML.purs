module Data.XML where

import Control.Alternative ((<|>))
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (fromString) as Data.Int
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (StrMap, empty, insert)
import Data.String (singleton)
import Data.String.Unsafe (char) as Data.String.Unsafe
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.Combinators (lookAhead, many1Till, manyTill, try)
import Text.Parsing.Parser.String (anyChar, satisfy, skipSpaces, string)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (between)

data XML
  = XMLNode String (StrMap String) (Maybe (Array XML))
  | XMLContent String

class EncodeXml a where
  encodeXml :: a -> XML

instance encodeXmlString :: EncodeXml String where
  encodeXml a = XMLContent a

instance encodeXmlNumber :: EncodeXml Number where
  encodeXml a = XMLContent (show a)

instance encodeXmlInt :: EncodeXml Int where
  encodeXml a = XMLContent (show a)

instance encodeXmlBool :: EncodeXml Boolean where
  encodeXml a = XMLContent (show a)

class DecodeXml a where
  decodeXml :: XML -> Either String a

instance decodeXmlString :: DecodeXml String where
  decodeXml (XMLNode _ _ _) = Left "expected a string"
  decodeXml (XMLContent a) = Right a

instance decodeXmlNumber :: DecodeXml Number where
  decodeXml (XMLNode _ _ _) = Left "expected a float string"
  decodeXml (XMLContent a) = maybe (Left "expected string to be float") Right (unsafeCoerce a)

instance decodeXmlInt :: DecodeXml Int where
  decodeXml (XMLNode _ _ _) = Left "expected an integer string"
  decodeXml (XMLContent a) = maybe (Left "expected string to be integer") Right (Data.Int.fromString a)

instance decodeXmlBool :: DecodeXml Boolean where
  decodeXml (XMLNode _ _ _) = Left "expected a boolean string"
  decodeXml (XMLContent "true") = Right true
  decodeXml (XMLContent "false") = Right false
  decodeXml (XMLContent _) = Left "expected string to be \"true\" or \"false\""

parseXml :: String -> Either ParseError XML
parseXml = flip runParser xml

xml :: Parser String XML
xml = do
  skipSpaces
  tagname <- openingTagName
  attrs <- tagAttributes
  let node = XMLNode tagname attrs
  try (selfClosingTag node) <|> tagWithChildren node

openingTagName :: Parser String String
openingTagName = do
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

tagWithChildren :: (Maybe (Array XML) -> XML) -> Parser String XML
tagWithChildren f = do
  void (string ">")
  try (pure <<< f <<< Just <<< fromFoldable =<< many1Till xml closingTag) <|> contentNode

contentNode :: Parser String XML
contentNode = (XMLContent <<< foldl append "") <$> many1Till anyButBracket closingTag

anyButBracket :: Parser String String
anyButBracket = singleton <$> (satisfy \c -> Data.String.Unsafe.char "<" /= c)

selfClosingTag :: ∀ a. (Maybe a -> XML) -> Parser String XML
selfClosingTag f = do
  let orphan = do
        void $ string ">"
        skipSpaces
        closingTag
  void (string "/>") <|> orphan
  pure $ f Nothing

tagEnd :: Parser String String
tagEnd = string " " <|> (lookAhead (string "/" <|> string ">"))
