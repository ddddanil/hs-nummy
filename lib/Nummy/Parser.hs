module Nummy.Parser (parse_nummy, parse_all) where

import GHC.Base (String)
import Protolude
import Text.Parsec as P hiding ( (<|>) )
import Text.Parsec.String as P.String

import Nummy.Parser.Units
import Nummy.Metrology.Show

parse_all :: Parser a -> Parser a
parse_all p = do
  x <- p
  _ <- eof
  return x

parse_nummy :: Parser String
parse_nummy = showQu <$> parse_all quantity
