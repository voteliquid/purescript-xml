module Data.XML.Parse (parseXML) where

import Control.Alternative ((<|>))
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State.Trans (get)
import Data.Array (fromFoldable, many, (:))
import Data.Char.Unicode (isAlpha, isAlphaNum)
import Data.Either (Either)
import Data.List (List(..), reverse)
import Data.String (fromCharArray)
import Data.String.Unsafe (char) as Data.String.Unsafe
import Data.Tuple (Tuple(..))
import Data.XML.Types (XML(..), XMLAttribute)
import Text.Parsing.Parser (ParseError, ParseState(..), Parser, fail, runParser)
import Text.Parsing.Parser.Combinators (lookAhead, try, withErrorMessage)
import Text.Parsing.Parser.String (anyChar, char, eof, noneOf, oneOf, satisfy, skipSpaces, string)
import Prelude hiding (between)

parseXML :: String -> Either ParseError XML
parseXML = flip runParser do
  skipSpaces
  _ <- try parseXmlDeclaration
  skipSpaces
  x <- try parseEmptyTag <|> parseClosedTag
  eof
  pure x

parseClosedTag :: Parser String XML
parseClosedTag = do
  _ <- string "<"
  n <- parseTagName
  skipSpaces
  attrs <- parseAttributes
  skipSpaces
  _ <- string ">"
  skipSpaces
  kids <- manyTill (parseCDATA <|> parseEmptyTag <|> parseClosedTag <|> parseContent) (string "</")
  skipSpaces
  _ <- string n
  skipSpaces
  _ <- string ">"
  skipSpaces
  pure $ XMLNode n attrs kids

parseEmptyTag :: Parser String XML
parseEmptyTag = do
  _ <- string "<"
  n <- parseTagName
  skipSpaces
  attrs <- parseAttributes
  skipSpaces
  _ <- string "/>"
  skipSpaces
  pure $ XMLNode n attrs Nil

parseXmlDeclaration :: Parser String XML
parseXmlDeclaration = do
  _ <- string "<?xml"
  skipSpaces
  attrs <- parseAttributes
  skipSpaces
  _ <- string "?>"
  skipSpaces
  pure $ XMLNode "xml" attrs Nil

parseTagName :: Parser String String
parseTagName = do
  a <- withErrorMessage (satisfy isAlpha <|> char (ch "_"))
        "tag name to begin with alpha character or underscore"
  as <- withErrorMessage (many $ satisfy isAlphaNum <|> oneOf [ch "-", ch "_", ch ":", ch "."])
        "tag name to contain only alphanumeric characters, dashes, underscores, colons, and periods"
  pure $ fromCharArray (a:as)

parseAttributes :: Parser String (List XMLAttribute)
parseAttributes = manyTill parseAttribute (lookAhead (string "?>" <|> string "/>" <|> string ">"))

parseAttribute :: Parser String XMLAttribute
parseAttribute = do
  let name = fromCharArray <$> (many $ satisfy isAlphaNum <|> oneOf [ch "-", ch "_", ch ":", ch "."])
  n <- withErrorMessage name
        "attribute name to contain only alphanumeric characters, dashes, underscores, colons, and periods"
  _ <- string "=\""
  v <- fromCharArray <$> many (noneOf [ch "\""])
  _ <- string "\""
  skipSpaces
  pure (Tuple n v)

parseCDATA :: Parser String XML
parseCDATA = do
  _ <- string "<![CDATA["
  cdata <- fromCharArray <<< fromFoldable <$> manyTill anyChar (lookAhead (string "]]>"))
  _ <- string "]]>"
  pure (XMLContent cdata)

parseContent :: Parser String XML
parseContent = pure <<< XMLContent =<< fromCharArray <$> many (noneOf [ch "<"])

logPos :: Parser String Unit
logPos = do
  ps@(ParseState str pos _) <- get
  _ <- fail str
  pure unit

-- | Tail-recursive implementation of manyTill.
manyTill :: forall a end. Parser String a -> Parser String end -> Parser String (List a)
manyTill p end = (end *> pure Nil) <|> many1Till p end

-- | Tail-recursive implementation of many1Till.
many1Till :: forall a end. Parser String a -> Parser String end -> Parser String (List a)
many1Till p end = do
  x <- p
  tailRecM inner (pure x)
  where
    ending acc = do
      _ <- end
      pure $ Done (reverse acc)
    continue acc = do
      c <- p
      pure $ Loop (Cons c acc)
    inner acc = ending acc <|> continue acc

ch :: String -> Char
ch = Data.String.Unsafe.char
