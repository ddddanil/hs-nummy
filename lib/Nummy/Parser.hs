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
, ParserOptions (..)
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
import Nummy.Parser.Command
import Nummy.Parser.Unit

-- | Parse input into an answer
parse_nummy :: Parser c (ParserResult c)
parse_nummy = parse_command <|> parse_physical where
  parse_physical = PResult . annotate SResult . prettyQu <$> physical <* space <* eof
  parse_command = PCommand <$> command <* space <* eof

-- | Top level parser
nummy :: ParserOptions cmd -> Text -> ParserResult cmd
nummy o t = either PError identity $ runReader (runParserT parse_nummy "<input>" t) o
