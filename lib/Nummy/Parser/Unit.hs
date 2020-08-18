module Nummy.Parser.Unit (
  unit
) where

import Nummy.Prelude hiding (many, Prefix, try)
import Control.Lens
import Data.Char (isAlpha)
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import Control.Monad.Fail

import Nummy.Parser.Base
import Nummy.Metrology
import Nummy.Metrology.Definitions.Unit (scalar_unit)


-- Term

pBaseUnit :: Parser c Unit
pBaseUnit = do
  str <- takeWhile1P (Just "base unit") isAlpha
  lookupU <- lift $ asks (^. lookupUnit)
  case lookupU str of
    Just u -> return u
    Nothing -> fail "Not a known base unit"


-- Operators

pOpUnitPow :: Parser c (Unit -> Unit)
pOpUnitPow = do
  _ <- char '^'
  v <- pValue
  return $ \u -> u #^ v

pOpUnitMultC :: Parser c (Unit -> Unit -> Unit)
pOpUnitMultC = do
  _ <- char '*'
  return (#*)

pOpUnitMultS :: Parser c (Unit -> Unit -> Unit)
pOpUnitMultS = try $ do
  _ <- char ' '
  _ <- lookAhead pBaseUnit
  return (#*)

pOpUnitDiv :: Parser c (Unit -> Unit -> Unit)
pOpUnitDiv = do
  _ <- char '/'
  return (#/)

pOpUnitInv :: Parser c (Unit -> Unit)
pOpUnitInv = do
  _ <- string "1/"
  return $ ((#/) (scalar_unit))


-- Operator table

opUnitTable :: [[Operator (Parser c) Unit]]
opUnitTable =
  [ [ Postfix pOpUnitPow   ]
  , [ InfixL  pOpUnitMultS ]
  , [ Prefix  pOpUnitInv   ]
  , [ InfixL  pOpUnitDiv   ]
  , [ InfixL  pOpUnitMultC ]
  ]


-- Expression builder

-- | Parser for a unit expression
--
-- Base terms: units with prefixes
-- Operators: @\'^\', \' \', \'*\', \'/\', \"1/\"@
--
-- @\' \'@ binds tighter than @\'*\'@
--
-- Examples:
--
-- > m
-- > kg
-- > km/h
-- > N ms
unit :: Parser c Unit
unit = makeExprParser pBaseUnit opUnitTable
