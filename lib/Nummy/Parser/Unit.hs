module Nummy.Parser.Unit (
  unit
) where

import Nummy.Prelude hiding (many, Prefix, try)
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr

import Nummy.Parser.Base
import Nummy.Metrology
import Nummy.Metrology.Definitions (comboTable)
import Nummy.Metrology.Definitions.Unit (dimless)


-- Term

pBaseUnit :: Parser Unit
pBaseUnit = choice (map parserify comboTable) <?> "base unit" where
  parserify :: (Label, Unit) -> Parser Unit
  parserify (s, u) = do
    _ <- string s
    notFollowedBy alphaNumChar
    return u


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
  lookAhead pBaseUnit
  return (#*)

pOpUnitDiv :: Parser (Unit -> Unit -> Unit)
pOpUnitDiv = do
  _ <- char '/'
  return (#/)

pOpUnitInv :: Parser (Unit -> Unit)
pOpUnitInv = do
  _ <- string "1/"
  return $ ((#/) (dimless))


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
