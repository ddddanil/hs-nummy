module Nummy.Parser (parse_nummy) where

import GHC.Base (String)
import Protolude
import Text.Parsec as P hiding ( (<|>) )
import Text.ParserCombinators.Parsec.Char as P.Char
import Text.ParserCombinators.Parsec.Prim as P.Prim hiding ( (<|>) )
import Text.ParserCombinators.Parsec.Combinator as P.Comb
import Text.ParserCombinators.Parsec.Number as P.Number (floating2)

import Nummy.Parser.Units

parse_nummy = unit >> eof
