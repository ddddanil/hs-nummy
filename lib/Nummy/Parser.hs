{-|
Module        : Nummy.Parser
Description   : Parsers for units, quantities and their expressions
Maintainer    : ddddanil@vivaldi.net
Stability     : experimental
-}

module Nummy.Parser (
  Parser
, ParserError
, ParseExcept
, nummy
, unit
, quantity
, expression
, physical
, parse_nummy
) where

import Nummy.Prelude
import Control.Monad.Except (liftEither)
import Text.Megaparsec
import Text.Megaparsec.Char (space)
import Data.Text.Prettyprint.Doc (pretty)

import Nummy.Parser.Base
import Nummy.Parser.Expr
import Nummy.Parser.Unit

-- | Parse input into an answer
parse_nummy :: Parser Text
parse_nummy = parse_physical where
  parse_physical = show . pretty <$> physical <* space <* eof

nummy :: Text -> ParseExcept Text
nummy t = liftEither =<< (lift $ runParserT parse_nummy "<input>" t)
