module Nummy.Parser.Unit (
  unit
) where

import Nummy.Prelude hiding (many, Prefix, try)
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Data.Char (isAlpha)
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import Control.Monad.Fail

import Nummy.Parser.Base
import Nummy.Metrology
import Nummy.Metrology.Definitions (lookupUnit)
import Nummy.Metrology.Definitions.Unit (scalar_unit)


-- Term

pBaseUnit :: Parser Unit
pBaseUnit = do
  str <- takeWhile1P (Just "base unit") isAlpha
  mu <- lift . runMaybeT $ lookupUnit Nothing str
  case mu of
    Just u -> return u
    Nothing -> fail "Not a known base unit"


-- Operators

pOpUnitPow :: Parser (Unit -> Unit)
pOpUnitPow = do
  _ <- char '^'
  v <- pValue
  return $ \u -> u #^ v

pOpUnitMultC :: Parser (Unit -> Unit -> Unit)
pOpUnitMultC = do
  _ <- char '*'
  return (#*)

pOpUnitMultS :: Parser (Unit -> Unit -> Unit)
pOpUnitMultS = try $ do
  _ <- char ' '
  _ <- lookAhead pBaseUnit
  return (#*)

pOpUnitDiv :: Parser (Unit -> Unit -> Unit)
pOpUnitDiv = do
  _ <- char '/'
  return (#/)

pOpUnitInv :: Parser (Unit -> Unit)
pOpUnitInv = do
  _ <- string "1/"
  return $ ((#/) (scalar_unit))


-- Operator table

opUnitTable :: [[Operator Parser Unit]]
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
unit :: Parser Unit
unit = makeExprParser pBaseUnit opUnitTable
