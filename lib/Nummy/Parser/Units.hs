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

import Nummy.Metrology.Definitions
import Nummy.Metrology.Dimension
import Nummy.Metrology.Unit

oneOfStr :: [[Char]] -> Parser [Char]
oneOfStr ss = choice $ map (try . string) . sortBy (flip compare `on` length) $ ss


-- Dimension

baseUnit :: Parser Unit
baseUnit = P.option dimlessUnit $ choice
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


fullUnitOpTable :: (Monad m) => OperatorTable String () m Unit
fullUnitOpTable =
  [ [ Infix (char '^' >> return pow_op ) AssocLeft ]
  , [ Infix (char ' ' >> return (#*) ) AssocLeft ]
  , [ Infix (char '/' >> return (#/) ) AssocLeft ]
  , [ Infix (char '*' >> return (#*) ) AssocLeft ]
  ]
  where pow_op = (fromJust.) . (#^)

shortUnitOpTable :: (Monad m) => OperatorTable String () m Unit
shortUnitOpTable =
  [ [ Infix (char '^' >> return pow_op ) AssocLeft ]
  , [ Infix (char '*' >> return (#*) ) AssocLeft ]
  , [ Infix (char '/' >> return (#/) ) AssocLeft ]
  ]
  where pow_op = (fromJust.) . (#^)

unitExpr :: OperatorTable String () Identity Unit -> Parser Unit
unitExpr table = buildExpressionParser table $
            choice [ try baseUnit, try dimlessQu ]
        where dimlessQu = rawValue >>= \v -> return $ mkQu v dimlessUnit


unit :: Parser Unit
unit = try (brackets $ unitExpr fullUnitOpTable) <|> try (unitExpr shortUnitOpTable) where
  brackets = between (char '(') (char ')')

-- Quantity

modifiedValue :: Parser Value
modifiedValue = do
  n <- floating2 False :: Parser Double
  let rn = toRational n
  maym <- optionMaybe modifier
  case maym of
    Nothing -> return rn
    Just m -> let m' = fromJust $ lookupUnit (Just $ baseDim Modifier) m
              in return $ applyModifier m' rn
  where modifier = oneOfStr modifierTable

rawValue :: Parser Value
rawValue = do
  n <- floating2 False :: Parser Double
  return $ toRational n

quantity :: Parser Quantity
quantity = choice [try wideQu, slimQu] where
  wideQu = do
    v <- modifiedValue
    _ <- space
    u <- unit
    guard . not . isDimless . fst $ u
    return $ mkQu v u
  slimQu = do
    v <- rawValue
    u <- unit
    return $ mkQu v u
