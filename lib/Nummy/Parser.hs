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
, physical
) where

import Nummy.Prelude
import Text.Megaparsec
import Data.Text.Prettyprint.Doc (pretty)

import Nummy.Parser.Base
import Nummy.Parser.Expr
import Nummy.Parser.Unit

-- | Parse input into an answer
parse_nummy :: Parser Text
parse_nummy = parse_physical where
  parse_physical = show . pretty <$> physical <* eof
