module Nummy.Parser.Units (
  unit, quantity
) where

import Protolude hiding (Prefix, Infix, try)
import Data.String (String)
import Data.Maybe (fromJust)
import Data.Ratio (approxRational)
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

parseBaseUnit = (oneOfStr baseUnitTable <?> "known unit symbol") >>= \u -> parseMaybe (lookupUnit Nothing u) <?> "known unit symbol"
parsePrefix = (oneOfStr prefixTable <?> "known prefix") >>= \p -> parseMaybe (lookupPrefix p) <?> "known prefix"
parseModifier = (oneOfStr modifierTable <?> "known modifier") >>= \m -> parseMaybe (lookupModifier m) <?> "known modifier"

parenthesis = between (char '(' >> spaces >> notFollowedBy space) (spaces >> char ')')


-- Unit parser

baseUnit :: Parser Unit
baseUnit = choice
  [ try $ do {
      p <- parsePrefix;
      u <- parseBaseUnit;
      return $ applyPrefix p u;
    }
  , parseBaseUnit
  , parseDimlessCoeff
  ]

parseDimlessCoeff :: Parser Unit
parseDimlessCoeff = do
  v <- rawValue
  return $ dimlessCoeff v

-- add pow ^ later
fullUnitOpTable :: OperatorTable String () Identity Unit
fullUnitOpTable =
  [ [ Infix (char '^' >> lookAhead parseDimlessCoeff >> return pow) AssocLeft ]
  , [ Infix (char ' ' >> notFollowedBy space >> return (#*) ) AssocRight ]
  , [ Infix (char '/' >> return (#/) ) AssocLeft ]
  , [ Infix (char '*' >> return (#*) ) AssocLeft ]
  ]
  where
    pow :: Unit -> Unit -> Unit
    pow u1 u2 = fromJust $ (u1 #!^ u2)

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
unit = try (parenthesis $ unitExpr fullUnitOpTable) <|> try (unitExpr shortUnitOpTable) <?> "unit"


-- Quantity parser

modifiedValue :: Parser Value
modifiedValue = do
  n <- rawValue
  m <- parseModifier
  return $ applyModifier m n

rawValue :: Parser Value
rawValue = do
  n <- floating2 False :: Parser Double
  return $ approxRational n epsilon
  where epsilon = 0.000001

quantity :: Parser Quantity
quantity = try wideQu <|> try slimQu <|> dimlessQu <?> "quantity" where
  wideQu = do
    v <- try modifiedValue <|> rawValue
    _ <- space
    u <- unit
    guard . not . isDimless . dimOfUnit $ u
    return $ mkQu v u
  slimQu = do
    v <- rawValue
    u <- unit
    return $ mkQu v u
  dimlessQu = do
    v <- try modifiedValue <|> rawValue
    return $ mkQu v dimlessUnit
