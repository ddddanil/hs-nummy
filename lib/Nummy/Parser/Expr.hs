module Nummy.Parser.Expr (
  quantity
, expression
) where

import Protolude hiding (Prefix, Infix, try, optional)
import Data.Maybe (fromJust)
import Text.Parsec as P hiding ( (<|>) )
import Text.Parsec.Expr as P.Expr

import Nummy.Parser.Base
import Nummy.Parser.Unit
import Nummy.Metrology as M
import Nummy.Metrology.Definitions (dimless_unit)


-- Quantity operations

quOpMult :: Parser (Quantity -> Quantity -> Quantity)
quOpMult = spaces >> char '*' >> spaces >> return (%*)

quOpDiv :: Parser (Quantity -> Quantity -> Quantity)
quOpDiv = spaces >> char '/' >> spaces >> return (%/)

quOpSum :: Parser (Quantity -> Quantity -> Quantity)
quOpSum = do
  _ <- spaces >> char '+' >> spaces
  next <- lookAhead quantity
  guardDim (dimOfQu next) <?> "Quantities must be of same dimension"
  return $ (fromJust.) . (%+)

quOpDif :: Parser (Quantity -> Quantity -> Quantity)
quOpDif = do
  _ <- spaces >> char '-' >> spaces
  next <- lookAhead quantity
  guardDim (dimOfQu next) <?> "Quantities must be of same dimension"
  return $ (fromJust.) . (%-)

quOpNegate :: Parser (Quantity -> Quantity)
quOpNegate = do
  _ <- char '-'
  return $ \q -> ((-1) %# dimless_unit) %* q

quOpIn :: Parser (Quantity -> Quantity)
quOpIn = do
  _ <- spaces
  _ <- string "in" <|> string "into" <|> string "of"
  _ <- space >> spaces
  u <- unit
  guardDim (dimension u) <?> "Specifier must be of same dimension"
  return $ \q -> fromJust (q %<| u)


quOpTable :: OpTable Quantity
quOpTable =
  [ [ Prefix (try quOpNegate) ]
  , [ Infix (try quOpMult) AssocLeft ]
  , [ Infix (try quOpDiv) AssocLeft ]
  , [ Infix (try quOpSum) AssocLeft ,  Infix (try quOpDif) AssocLeft ]
  ]


-- Quantity parsers

-- | Parses either a marked quantity or a scalar value
--
-- Examples:
--
-- @
-- 5
-- 10m
-- 4 m
-- 9m/s
-- 3 (kg m/s^2)
-- @
quantity :: Parser Quantity
quantity = try marked <|> try scalar <?> "quantity" where
  marked = do
    v <- parseValue
    _ <- optional space
    u <- unit
    guard . not . (== dimless_unit) $ u
    return $ v %# u
  scalar = do
    v <- parseValue
    return $ v %# dimless_unit


parseQuantity :: Parser Quantity
parseQuantity = do
  q <- quantity
  putDim $ dimOfQu q
  return q

exprParser :: Parser Quantity
exprParser = buildExpressionParser quOpTable parseQuantity

-- | Parses an expression and attempts to convert it into an optional specifier
expression :: Parser Quantity
expression = do
  _ <- spaces
  q <- exprParser
  putDim $ dimOfQu q
  conv <- optionMaybe quOpIn
  _ <- spaces
  return $ maybe q ((&) q) conv
