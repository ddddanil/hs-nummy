module Nummy.Parser (parse_nummy) where

import GHC.Base (String)
import Protolude
import Text.Parsec as P hiding ( (<|>) )
import Text.Parsec.String as P.String

import Nummy.Parser.Units
import Nummy.Metrology.Show

parse_nummy :: Parser String
parse_nummy = do
  qu <- quantity
  _ <- eof
  return $ showQu qu
