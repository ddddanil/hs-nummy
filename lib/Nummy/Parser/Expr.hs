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
import Nummy.Metrology.Definitions.Unit (dimless)


-- Quantity operations

quOpMult :: Parser (Maybe Quantity -> Maybe Quantity -> Maybe Quantity)
quOpMult = spaces >> char '*' >> spaces >> return (\a b -> (%*) <$> a <*> b)

quOpDiv :: Parser (Maybe Quantity -> Maybe Quantity -> Maybe Quantity)
quOpDiv = spaces >> char '/' >> spaces >> return (\a b -> (%/) <$> a <*> b)

quOpSum :: Parser (Maybe Quantity -> Maybe Quantity -> Maybe Quantity)
quOpSum = do
  _ <- spaces >> char '+' >> spaces
  return $ \a b -> concatMaybe ( (%+) <$> a <*> b )

quOpDif :: Parser (Maybe Quantity -> Maybe Quantity -> Maybe Quantity)
quOpDif = do
  _ <- spaces >> char '-' >> spaces
  return $ \a b -> concatMaybe ( (%-) <$> a <*> b )

quOpNegate :: Parser (Maybe Quantity -> Maybe Quantity)
quOpNegate = do
  _ <- char '-'
  return $ fmap $ \q -> ((-1) %# dimless) %* q

quOpIn :: Parser (Maybe Quantity -> Maybe Quantity)
quOpIn = do
  _ <- string "in" <|> string "into" <|> string "to"
  _ <- space >> spaces
  u <- unit
  return $ \a -> a >>= (%<| u)


quOpTable :: OpTable (Maybe Quantity)
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
    guard . not . (== dimless) $ u
    notFollowedBy alphaNum
    return $ v %# u
  scalar = do
    v <- parseValue
    return $ v %# dimless


exprParser :: Parser (Maybe Quantity)
exprParser = buildExpressionParser quOpTable (Just <$> quantity)

-- | Parses an expression and attempts to convert it into an optional specifier
expression :: Parser Quantity
expression = do
  _ <- spaces
  mq <- exprParser
  when (isNothing mq) $ parserFail "Dimensions are not consistent"
  conv <- optionMaybe quOpIn
  _ <- spaces
  let mres = concatMaybe $ conv <*> pure mq
  case mres <|> mq of
    Just q -> return q
    Nothing -> parserFail "Specifier dimension is not consistent"
