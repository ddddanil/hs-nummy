module Nummy.Parser (parse_nummy) where

import GHC.Base (String)
import Protolude
import Text.Parsec as P hiding ( (<|>) )
import Text.Parsec.String as P.String

import Nummy.Parser.Units

parse_nummy = do
  qu <- quantity
  _ <- eof
  return qu
