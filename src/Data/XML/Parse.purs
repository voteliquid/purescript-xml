module Data.XML.Parse (parseXML) where

import Control.Alternative ((<|>))
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.Map (empty, insert)
import Data.String (singleton)
import Data.String.Unsafe (char) as Data.String.Unsafe
import Data.Tuple (Tuple(..))
import Data.XML.Types (XML(..), XMLAttributes)
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.Combinators (lookAhead, many1Till, manyTill, try)
import Text.Parsing.Parser.String (anyChar, satisfy, skipSpaces, string)
import Prelude hiding (between)

parseXML :: String -> Either ParseError XML
parseXML = flip runParser $ try (parseDeclaration >>= \_ -> parseNode) <|> parseNode

parseNode :: Parser String XML
parseNode = do
  tagname <- parseOpeningTagName
  attrs <- parseparseAttributes
  let node = XMLNode tagname attrs
  (try (parseContentNode node)) <|> (try (parseSelfClosingTag node)) <|> (parseChildNodes node)

parseDeclaration :: Parser String Unit
parseDeclaration = do
  skipSpaces
  void $ string "<?"
  void $ manyTill (singleton <$> anyChar) (string "?>")

parseOpeningTagName :: Parser String String
parseOpeningTagName = do
  skipSpaces
  void (string "<")
  foldl append "" <$> many1Till (singleton <$> anyChar) parseTagEnd

parseparseAttributes :: Parser String XMLAttributes
parseparseAttributes = foldl (\b (Tuple k v) -> insert k v b) empty <$> manyTill parseAttribute parseTagEnd

parseAttribute :: Parser String (Tuple String String)
parseAttribute = do
  key <- parseAttributeKey
  val <- parseAttributeValue
  pure $ Tuple key val

parseAttributeValue :: Parser String String
parseAttributeValue = foldl append "" <$> many1Till (singleton <$> anyChar) (string "\"")

parseAttributeKey :: Parser String String
parseAttributeKey = foldl append "" <$> many1Till (singleton <$> anyChar) (string "=\"")

parseClosingTag :: Parser String Unit
parseClosingTag = do
  void $ string "</"
  skipSpaces
  void $ manyTill (singleton <$> anyChar) (skipSpaces >>= \_ -> string ">")
  skipSpaces

parseChildNodes :: (List XML -> XML) -> Parser String XML
parseChildNodes f = do
  void (string ">")
  pure <<< f =<< many1Till parseNode parseClosingTag

parseCDATA :: Parser String String
parseCDATA = do
  void $ string "<![CDATA["
  str <- foldl append "" <$> many1Till (singleton <$> anyChar) (string "]]>")
  parseClosingTag
  pure str

parseContentNode :: (List XML -> XML) -> Parser String XML
parseContentNode f = do
  void (string ">")
  content <- try parseCDATA <|> (foldl append "" <$> many1Till anyButBracket parseClosingTag)
  skipSpaces
  pure $ f $ pure (XMLContent content)

parseSelfClosingTag :: (List XML -> XML) -> Parser String XML
parseSelfClosingTag f = do
  let orphan = do
        void $ string ">"
        skipSpaces
        parseClosingTag
  skipSpaces
  void (string "/>") <|> orphan
  skipSpaces
  pure $ f Nil

parseTagEnd :: Parser String String
parseTagEnd = string " " <|> (lookAhead (string "/" <|> string ">"))

anyButBracket :: Parser String String
anyButBracket = singleton <$> (satisfy \c -> Data.String.Unsafe.char "<" /= c && Data.String.Unsafe.char ">" /= c)
