module Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Either (Either(..))
import Data.XML (parseXml)
import Text.Parsing.Parser (parseErrorMessage, parseErrorPosition, runParser)
import Unsafe.Coerce (unsafeCoerce)

main =
  case parseXml "<fuck foo=\"b\"><bar/><baz><foobar>something</foobar></baz></fuck>" of
    Left err -> do
      log (parseErrorMessage err)
      log ("position: " <> show (parseErrorPosition err))
      pure (unsafeCoerce unit)
    Right xml -> pure xml

