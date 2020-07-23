module Nummy.Parser (parse_nummy) where

import GHC.Base (String)
import Protolude
import Text.Parsec as P hiding ( (<|>) )

import Nummy.Parser.Units

parse_nummy = quantity >> eof
