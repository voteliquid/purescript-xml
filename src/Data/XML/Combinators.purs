module Data.XML.Combinators
  ( xmlChild
  , (?>)
  , xmlChildren
  , (?>>)
  , xmlTextChild
  , (=?>)
  , xmlNumChild
  , (#?>)
  , xmlIntChild
  , (!?>)
  ) where

import Prelude
import Data.Array (filter, index)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.XML.Types (XML(..))
import Global (isFinite, readFloat)

xmlChild :: XML -> String -> Either String XML
xmlChild (XMLContent _) tag = Left $ "expected an xml node but found content node"
xmlChild (XMLNode _ _ Nothing) tag = Left "expected a node with children"
xmlChild (XMLNode n _ (Just kids)) tag =
  let fold b node@(XMLNode name _ _) = if name == tag then Right node else b
      fold b node@(XMLContent _) = b
   in foldl fold (Left ("no node found with tag \"" <> tag <> "\"")) kids

infix 7 xmlChild as ?>

xmlChildren :: XML -> String -> Either String (Array XML)
xmlChildren (XMLContent _) _ = Left "expected an xml node but found content node"
xmlChildren (XMLNode _ _ Nothing) tag = Left "expected a node with children"
xmlChildren (XMLNode n _ (Just kids)) tag =
  let check (XMLNode name _ _) = name == tag
      check (XMLContent _) = false
   in Right $ filter check kids

infix 7 xmlChildren as ?>>

xmlTextChild :: XML -> String -> Either String String
xmlTextChild x s = xmlChild x s >>= xmlText

infix 7 xmlTextChild as =?>

xmlNumChild :: XML -> String -> Either String Number
xmlNumChild x s = xmlTextChild x s >>= xmlNum

infix 7 xmlNumChild as #?>

xmlIntChild :: XML -> String -> Either String Int
xmlIntChild x s = xmlTextChild x s >>= xmlInt

infix 7 xmlIntChild as !?>

xmlText :: XML -> Either String String
xmlText (XMLContent _) = Left "expected an xml node but found content node"
xmlText (XMLNode _ _ Nothing) = Left "expected a node with children"
xmlText (XMLNode _ _ (Just kids)) =
  let fold txt (XMLContent s) = txt <> s
      fold txt (XMLNode _ _ _) = txt
   in Right $ foldl fold "" kids

xmlNum :: String -> Either String Number
xmlNum = maybe (Left "expected a valid float string") Right <<< check <<< readFloat
  where
    check num | isFinite num = Just num
              | otherwise = Nothing

xmlInt :: String -> Either String Int
xmlInt = maybe (Left "expected a valid integer string") Right <<< Int.fromString

xmlBool :: String -> Either String Boolean
xmlBool "true" = Right true
xmlBool "false" = Right false
xmlBool _ = Left "expected a valid bool string"

xmlChildAtIdx :: XML -> String -> Int -> Either String XML
xmlChildAtIdx (XMLContent _) _ _ = Left "expected a node"
xmlChildAtIdx (XMLNode _ _ Nothing) _ _ = Left "expected a node with children"
xmlChildAtIdx (XMLNode _ _ (Just kids)) tag idx =
  let elem = index kids idx
      isTag (XMLNode name _ _) = name == tag
      isTag (XMLContent _) = false
   in case elem of
        Nothing -> Left $ "no node at index " <> (show idx)
        Just node ->
          if isTag node then Right node else Left ("node does not have tag \"" <> tag <> "\"")

infix 7 xmlChildAtIdx as @>
