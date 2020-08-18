module Nummy.Parser.Physical (
  quantity
, expression
, physical
) where

import Nummy.Prelude hiding (many, Prefix, try)
import Data.Maybe (fromJust)
import Text.Megaparsec (choice)
import Text.Megaparsec.Char
import Control.Monad.Fail
import Control.Monad.Combinators.Expr

import Nummy.Parser.Base
import Nummy.Parser.Unit
import Nummy.Metrology
import Nummy.Metrology.Definitions.Unit (scalar_unit)


-- Term

-- | Quantity parser
--
-- Attempts to bind an optional unit (maybe in parenthesis) to a value
--
-- Examples:
--
-- > 6
-- > 4m
-- > 8 Pa
-- > 9.8 m/s^2
quantity :: Parser c Quantity
quantity = do
  v <- pValue
  _ <- space
  u <- optional $ parenthesis unit <|> unit
  return $ maybe (v %# scalar_unit) ((%#) v) u


-- Operators

pOpQuPow :: Parser c (Maybe Quantity -> Maybe Quantity)
pOpQuPow = do
  _ <- char '^'
  _ <- space
  p <- pValue
  _ <- space
  return $ fmap (%^ p)

pOpQuNeg :: Parser c (Maybe Quantity -> Maybe Quantity)
pOpQuNeg = do
  _ <- char '-'
  return $ fmap (%* (-1) %# scalar_unit)

pOpQuMul :: Parser c (Maybe Quantity -> Maybe Quantity -> Maybe Quantity)
pOpQuMul = do
  _ <- char '*'
  _ <- space
  return $ \a b -> (%*) <$> a <*> b

pOpQuDiv :: Parser c (Maybe Quantity -> Maybe Quantity -> Maybe Quantity)
pOpQuDiv = do
  _ <- char '/'
  _ <- space
  return $ \a b -> (%/) <$> a <*> b

pOpQuAdd :: Parser c (Maybe Quantity -> Maybe Quantity -> Maybe Quantity)
pOpQuAdd = do
  _ <- char '+'
  _ <- space
  return $ \a b -> join $ (%+) <$> a <*> b

pOpQuSub :: Parser c (Maybe Quantity -> Maybe Quantity -> Maybe Quantity)
pOpQuSub = do
  _ <- char '-'
  _ <- space
  return $ \a b -> join $ (%-) <$> a <*> b


-- Operator table

opQuTable :: [[Operator (Parser c) (Maybe Quantity)]]
opQuTable =
  [ [ Prefix  pOpQuNeg ]
  , [ Postfix pOpQuPow ]
  , [ InfixL  pOpQuDiv ]
  , [ InfixL  pOpQuMul ]
  , [ InfixL  pOpQuSub, InfixL pOpQuAdd ]
  ]


-- Expression builder

-- | Parses a quantity expression
--
-- Returns 'Just' when all dimensions are consistent and 'Nothing' otherwise
expression :: Parser c (Maybe Quantity)
expression = makeExprParser (Just <$> quantity <* space) opQuTable


-- Line parser

pFormat :: Parser c (Quantity -> Maybe Quantity)
pFormat = do
  _ <- char '|'
  _ <- space
  choice -- Accept a '1' as a substitute for a scalar unit
    [ do { u <- unit; return $ (%<| u); }
    , do { _ <- char '1'; return $ (%<| scalar_unit); }
    ]

-- | Parser for an expression with physical units
--
-- Accepts a unit as its format. Fails if the dimensions are not consistent
physical :: Parser c Quantity
physical = do
  _ <- space
  mqu <- expression
  when (isNothing mqu) $ fail "Expression dimensions are not consistent"
  mform <- optional pFormat
  case mform of
    Just form -> do
      let res = mqu >>= form
      when (isNothing res) $ fail "Specifier dimension is not consistent"
      return $ fromJust res
    Nothing -> return $ fromJust mqu
