# purescript-xml

## Disclaimer: This is a work in progress. XPath is not completely implemented, and the XML parser is slow.

Provides an XML parser, and type classes and combinators for encoding and decoding XML.

## Usage

### Parse XML

```purescript
import Data.XML (parseXML)

xml :: Either String XML
xml = parseXML "<program><hello/><world/></program>"
```

### Decode XML to value

Given the following XML...

```xml
<user>
  <id>15dbb07f-92eb-42bb-8898-15df3c67898a</id>
  <loginAttempts>4</loginAttempts>
  <names>
    <firstName><language>english</language><text>John</text></firstName>
    <lastName><language>english</language><text>Backus</text></lastName>
  </names>
  <roles>
    <role>moderator</role>
    <role>admin</role>
  </roles>
  <created></created>
</user>
```

... We can decode the above into another value by implementing the `decodeXML
:: XML -> Either String a` function from the `Data.XML.Decode` type class.
Data is extracted from XML using [XPath](https://en.wikipedia.org/wiki/XPath)
queries. Currently, only LocationPath expressions, child axes, name tests, and
text node tests are supported.

```purescript
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.XML (class DecodeXML, decodeXML, (?>), (??>), (?>>))

newtype User = User
  { id :: String
  , login_attempts :: Int
  , name :: String
  , photo :: Maybe String
  , roles :: List String
  }

instance decodeXmlUser :: DecodeXML User where
  decodeXML xml = do
    -- `?>` extracts the first decoded child node from xpath query results
    id <- xml ?> "id/text()"

    -- XML nodes selected by the xpath query are decoded to primitive types, in this case integer.
    -- DecodeXML instances are provided for strings, numbers, and integers.
    login_attempts <- xml ?> "login_attempts/text()"

    name <- xml ?> "names/firstName/text()"

    -- `??>` extracts the first child node from xpath query result as a maybe value
    photo <- xml ??> "photo"

    -- `?>>` returns the list of xpath query result nodes
    roles <- xml ?>> "roles/role/text()"

    pure (User { id, name, photo, roles })
```

## Documentation

Documentation is published on [Pursuit](https://pursuit.purescript.org/packages/purescript-xml).
