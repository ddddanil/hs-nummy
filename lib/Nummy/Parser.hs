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
, NummyStyle (..)
, DocN
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
import Data.Text.Prettyprint.Doc (annotate)

import Nummy.Base
import Nummy.Metrology (prettyQu)
import Nummy.Parser.Base
import Nummy.Parser.Physical
import Nummy.Parser.Unit
import Nummy.Cache

-- | Parse input into an answer
parse_nummy :: Parser ParserResult
parse_nummy = parse_physical where
  parse_physical = PResult . annotate SResult . prettyQu <$> physical <* space <* eof

-- | Top level parser
nummy :: Text -> ReadCache ParserResult
nummy t = either PError identity <$> runParserT parse_nummy "<input>" t
