# purescript-xml

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

... We can decode the above into another value by implementing the
`decodeXML :: XML -> Either String a` function from the `Data.XML.Decode` type class:

```purescript
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.XML (class DecodeXML, decodeXML, (?>), (?>>), (=?>))

newtype User = User
  { id :: String
  , login_attempts :: Int
  , name :: String
  , photo :: Maybe String
  , roles :: List String
  }

instance decodeXmlUser :: DecodeXML User where
  decodeXML xml = do
    -- extract the first child node matching the tag name
    id <- xml ?> "id"

    -- decodes XML node containing string as integer (or number)
    login_attempts <- xml ?> "login_attempts"

    -- Drill into nested children with the `?>>` operator
    name <- xml ?> "names" ?>> "firstName" ?>> "text"

    -- decodes missing XML node as `Nothing`, and child of node as `Just a`
    photo <- xml ?> "photo"

    -- Lists and arrays are decoded
    -- `=?>` is used to return the selected XML node with its children filtered by a tag name
    roles <- xml ?> "roles" =?> "role"

    pure (User { id, name, photo, roles })
```

## Documentation

Documentation is published on [Pursuit](https://pursuit.purescript.org/packages/purescript-xml).
