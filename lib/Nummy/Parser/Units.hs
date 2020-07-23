module Nummy.Parser.Units (
  unit
) where

import Protolude hiding (Prefix, Infix, try)
import Data.Maybe (fromJust)
import Data.String (String)
import Text.Parsec as P hiding ( (<|>) )
import Text.Parsec.Char as P.Char
import Text.Parsec.Prim as P.Prim hiding ( (<|>) )
import Text.Parsec.Combinator as P.Comb
import Text.Parsec.Expr as P.Expr
import Text.Parsec.String as P.String

import Nummy.Metrology.Definitions

baseUnit :: Parser Unit
baseUnit = choice
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
  bare_unit = choice $ map string baseUnitTable

unit :: Parser Unit
unit = buildExpressionParser unitOpTable baseUnit

unitOpTable :: (Monad m) => OperatorTable String () m Unit
unitOpTable =
  [ [ Infix (char ' ' >> return (#*) ) AssocLeft ]
  , [ Infix (char '/' >> return (#/) ) AssocLeft ]
  , [ Infix (char '*' >> return (#*) ) AssocLeft ]
  ]
