module Nummy.Parser (
  Parser
, parse_nummy
, unit, quantity
, parse_all
) where

import GHC.Base (String)
import Protolude
import Text.Parsec as P hiding ( (<|>) )
import qualified Text.PrettyPrint.Leijen as PP

import Nummy.Parser.Base
import Nummy.Parser.Unit
import Nummy.Parser.Expr
import Nummy.Metrology.Unit
import Nummy.Metrology.Quantity

parse_all :: Parser a -> Parser a
parse_all p = do
  x <- p
  _ <- eof
  return x

parse_nummy :: String -> Either ParseError String
parse_nummy = second (show . PP.pretty) <$> runParser (parse_all expression) Nothing ""
