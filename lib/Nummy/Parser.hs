{-|
Module        : Nummy.Parser
Description   : Parsers for units, quantities and their expressions
Maintainer    : ddddanil@vivaldi.net
Stability     : experimental
-}

module Nummy.Parser (
-- * Parser
  Parser
, parse_nummy
-- * Units
-- $unit_types
, unit
-- * Expressions
, quantity
, expression
-- * Helper
, parse_all
) where

import GHC.Base (String)
import Protolude
import Text.Parsec as P hiding ( (<|>) )
import qualified Text.PrettyPrint.Leijen as PP

import Nummy.Parser.Base
import Nummy.Parser.Unit
import Nummy.Parser.Expr


-- $unit_types
-- A base unit is a combination of a unit synonym with a prefix. Base units
-- can be combined in expressions. All types of unit expressions allow
-- division (@'/'@), exponentials (@'^'@) and inversion (@"1/"@)
--
-- == Short unit expression
-- A short unit expression does not allow any spaces and uses @'*'@ as the main multiplication operator
--
-- Examples:
--
-- @
-- m
-- m/s
-- kg*m/s^2
-- @
--
-- == Long unit expression
-- Long unit expressions use a single space as the main multiplication operator
--
-- Examples:
--
-- @
-- kg m/s^2
-- @
--


-- | Forces a parser to consume all input
parse_all :: Parser a -> Parser a
parse_all p = do
  x <- p
  _ <- eof
  return x

-- | Attempt to parse an expression into an answer
parse_nummy :: String -> Either ParseError String
parse_nummy = second (show . PP.pretty) <$> runParser (parse_all expression) Nothing ""
