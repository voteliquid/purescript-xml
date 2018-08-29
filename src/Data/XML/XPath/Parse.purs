module Data.XML.XPath.Parse
  ( parseXPath
  ) where

import Control.Alternative ((<|>))
import Data.Array (many, (:))
import Data.Char.Unicode (isAlpha, isAlphaNum)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Data.String.Unsafe (char) as Data.String.Unsafe
import Data.XML.Types (Axis(..), LocationStep(..), NodeTest(..), XPath(..))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (sepBy, try, withErrorMessage)
import Text.Parsing.Parser.String (char, oneOf, satisfy, string)
import Prelude hiding (between)

parseXPath :: Parser String XPath
parseXPath = parseLocationPath

parseLocationPath :: Parser String XPath
parseLocationPath = do
  ps <- sepBy parseLocationStep (string "/")
  pure $ LocationPath ps

parseLocationStep :: Parser String LocationStep
parseLocationStep = do
  axis <- try parseChildAxis <|> pure Child
  node_test <- try parseTextNode <|> try parseAnyNode <|> parseNameTest
  pure (LocationStep axis node_test Nothing)

parseChildAxis :: Parser String Axis
parseChildAxis = do
  void $ string "child::"
  pure Child

parseNameTest :: Parser String NodeTest
parseNameTest = do
  n <- parseTagName
  pure (NameTest n)

parseTextNode :: Parser String NodeTest
parseTextNode = do
  void $ string "text()"
  pure TextNodeType

parseAnyNode :: Parser String NodeTest
parseAnyNode = do
  void $ string "*"
  pure AnyNodeType

parseTagName :: Parser String String
parseTagName = do
  a <- withErrorMessage (satisfy isAlpha <|> char (ch "_"))
        "tag name to begin with alpha character or underscore"
  as <- withErrorMessage (many $ satisfy isAlphaNum <|> oneOf [ch "-", ch "_", ch ":", ch "."])
        "tag name to contain only alphanumeric characters, dashes, underscores, colons, and periods"
  pure $ fromCharArray (a:as)

ch :: String -> Char
ch = Data.String.Unsafe.char
