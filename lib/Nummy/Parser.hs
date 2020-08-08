{-|
Module        : Nummy.Parser
Description   : Parsers for units, quantities and their expressions
Maintainer    : ddddanil@vivaldi.net
Stability     : experimental
-}

module Nummy.Parser (
  Parser
, ParserError
, ParserResult (..)
, nummy
, unit
, quantity
, expression
, physical
, parse_nummy
) where

import Nummy.Prelude
import Text.Megaparsec
import Text.Megaparsec.Char (space)
import Data.Text.Prettyprint.Doc (pretty)

import Nummy.Parser.Base
import Nummy.Parser.Physical
import Nummy.Parser.Unit
import Nummy.Cache (runReadCache)

-- | Parse input into an answer
parse_nummy :: Parser ParserResult
parse_nummy = parse_physical where
  parse_physical = PResult . show . pretty <$> physical <* space <* eof

nummy :: Text -> IO ParserResult
nummy t = fmap (either PError identity) . runReadCache $ runParserT parse_nummy "<input>" t
