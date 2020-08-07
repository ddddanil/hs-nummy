module Application.Parser where

import Nummy.Prelude
import Nummy.Parser
import qualified Data.Text as T (pack)
import Data.Bifoldable (bifold)
import Text.Megaparsec (errorBundlePretty)
import Nummy.Cache (runReadCache)

prettyParserError :: ParserError -> Text
prettyParserError = T.pack . errorBundlePretty

symbolParserError :: ParserError -> Text
symbolParserError = const "✗"

-- compactParserError :: ParserError -> Text
-- compactParserError =

runParser :: (ParserError -> Text) -> Text -> IO Text
runParser printer s =
  fmap bifold $
    runReadCache $
      runExceptT $
        printer `withExceptT` nummy s
