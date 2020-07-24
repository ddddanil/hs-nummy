module Nummy.Parser.Units (
  unit, quantity
) where

import Protolude hiding (Prefix, Infix, try)
import Data.String (String)
import Data.Maybe (fromJust)
import Text.Parsec as P hiding ( (<|>) )
import Text.Parsec.Char as P.Char
import Text.Parsec.Expr as P.Expr
import Text.Parsec.String as P.String
import Text.ParserCombinators.Parsec.Number as P.Number (floating2)

import Nummy.Metrology.Definitions hiding (length, mass, time)
import Nummy.Metrology.Dimension as Dimension
import Nummy.Metrology.Unit


-- Combinators

oneOfStr :: [[Char]] -> Parser [Char]
oneOfStr ss = choice $ map (try . string) . sortBy (flip compare `on` length) $ ss

parseMaybe :: Maybe a -> Parser a
parseMaybe Nothing = parserFail "Could not unpack Maybe"
parseMaybe (Just x) = return x

parseBaseUnit = oneOfStr baseUnitTable >>= \u -> parseMaybe (lookupUnit Nothing u) <?> "Could not look up a unit"
parsePrefix = oneOfStr prefixTable >>= \p -> parseMaybe (lookupPrefix p) <?> "Could not look up a prefix"
parseModifier = oneOfStr modifierTable >>= \m -> parseMaybe (lookupModifier m) <?> "Could not look up a modifier"


-- Unit parser

baseUnit :: Parser Unit
baseUnit = P.option dimlessUnit $ choice
  [ try $ do {
      p <- parsePrefix;
      u <- parseBaseUnit;
      return $ applyPrefix p u;
    }
  , parseBaseUnit
  ]

-- add pow ^ later
fullUnitOpTable :: (Monad m) => OperatorTable String () m Unit
fullUnitOpTable =
  [ [ Infix (char ' ' >> return (#*) ) AssocLeft ]
  , [ Infix (char '/' >> return (#/) ) AssocLeft ]
  , [ Infix (char '*' >> return (#*) ) AssocLeft ]
  ]

shortUnitOpTable :: (Monad m) => OperatorTable String () m Unit
shortUnitOpTable =
  [ [ Infix (char '*' >> return (#*) ) AssocLeft ]
  , [ Infix (char '/' >> return (#/) ) AssocLeft ]
  ]

unitExpr :: OperatorTable String () Identity Unit -> Parser Unit
unitExpr table = buildExpressionParser table baseUnit
            -- choice [ try baseUnit, try dimlessQu ]
        -- where dimlessQu = rawValue >>= \v -> return $ mkQu v dimlessUnit

unit :: Parser Unit
unit = try (brackets $ unitExpr fullUnitOpTable) <|> try (unitExpr shortUnitOpTable) where
  brackets = between (char '(' >> spaces) (spaces >> char ')')


-- Quantity parser

modifiedValue :: Parser Value
modifiedValue = do
  n <- rawValue
  m <- parseModifier
  return $ applyModifier m n

rawValue :: Parser Value
rawValue = do
  n <- floating2 False :: Parser Double
  return $ toRational n

quantity :: Parser Quantity
quantity = choice [try wideQu, try slimQu, dimlessQu] where
  wideQu = do
    v <- modifiedValue
    _ <- space
    u <- unit
    guard . not . isDimless . ofDim $ u
    return $ mkQu v u
  slimQu = do
    v <- rawValue
    u <- unit
    return $ mkQu v u
  dimlessQu = do
    v <- modifiedValue
    return $ mkQu v dimlessUnit
