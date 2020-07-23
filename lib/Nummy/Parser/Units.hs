module Nummy.Parser.Units (
  singleUnit
) where

import Protolude hiding (Prefix, Infix, try)
import Data.Maybe (fromJust)
import Text.Parsec as P hiding ( (<|>) )
import Text.Parsec.Char as P.Char
import Text.Parsec.Prim as P.Prim hiding ( (<|>) )
import Text.Parsec.Combinator as P.Comb
import Text.Parsec.Expr as P.Expr
import Text.Parsec.String as P.String

import Nummy.Metrology.Definitions

singleUnit :: Parser Unit
singleUnit = choice
  [ try $ do {
      p <- getUnit <$> prefix;
      u <- getUnit <$> bare_unit;
      return $ applyPrefix p u;
    }
  , try $ do {
      u <- bare_unit;
      return $ getUnit u;
    }
  ]
  where
  getUnit = fromJust . lookupUnit Nothing
  prefix = choice $ map string prefixTable
  bare_unit = choice $ map string unitTable
