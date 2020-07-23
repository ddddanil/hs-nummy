module Nummy.Parser.Units (
  unit, quantity
) where

import Protolude hiding (Prefix, Infix, try)
import Data.Maybe (fromJust)
import Data.String (String)
import Text.Parsec as P hiding ( (<|>) )
import Text.Parsec.Char as P.Char
import Text.Parsec.Expr as P.Expr
import Text.Parsec.String as P.String
import Text.ParserCombinators.Parsec.Number as P.Number (floating2)

import Nummy.Metrology.Definitions

oneOfStr :: [[Char]] -> Parser [Char]
oneOfStr ss = choice $ map (try . string) ss

-- Dimension

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
  prefix = oneOfStr prefixTable
  bare_unit = oneOfStr baseUnitTable

fullUnit :: Parser Unit
fullUnit = buildExpressionParser fullUnitOpTable baseUnit

fullUnitOpTable :: (Monad m) => OperatorTable String () m Unit
fullUnitOpTable =
  -- Add pow ^
  [ [ Infix (char ' ' >> return (#*) ) AssocLeft ]
  , [ Infix (char '/' >> return (#/) ) AssocLeft ]
  , [ Infix (char '*' >> return (#*) ) AssocLeft ]
  ]

shortUnit :: Parser Unit
shortUnit = buildExpressionParser shortUnitOpTable baseUnit

shortUnitOpTable :: (Monad m) => OperatorTable String () m Unit
shortUnitOpTable =
  [ [ Infix (char '*' >> return (#*) ) AssocLeft ]
  , [ Infix (char '/' >> return (#/) ) AssocLeft ]
  ]

unit :: Parser Unit
unit = try (brackets fullUnit) <|> try shortUnit where
  brackets = between (char '(') (char ')')

-- Quantity

numericalQu :: Parser Value
numericalQu = do
  n <- floating2 False :: Parser Double
  let rn = toRational n
  maym <- optionMaybe modifier
  case maym of
    Nothing -> return rn
    Just m -> let m' = fromJust $ lookupUnit (Just [("Modifier", 1)]) m
              in return $ applyModifier m' rn
  where modifier = oneOfStr modifierTable

quantity :: Parser Quantity
quantity = do
  v <- numericalQu
  _ <- space
  u <- unit
  return $ mkQu v u
