{-|
Module        : Nummy.Parser
Description   : Parsers for units, quantities and their expressions
Maintainer    : ddddanil@vivaldi.net
Stability     : experimental
-}

module Nummy.Parser (
  Parser
, parse_nummy
, unit
, quantity
, expression
, line
) where

import Nummy.Prelude
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text.Prettyprint.Doc (pretty)

import Nummy.Parser.Base
import Nummy.Parser.Expr
import Nummy.Parser.Unit

parse_nummy :: Parser Text
parse_nummy = show . pretty <$> line <* eof
