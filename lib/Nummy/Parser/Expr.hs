module Nummy.Parser.Expr (
  quantity
, expression
) where

import Protolude hiding (Prefix, Infix, try)
import Data.String (String)
import Data.Maybe (fromJust)
import Data.Ratio (approxRational)
import Text.Parsec as P hiding ( (<|>) )
import Text.Parsec.Char as P.Char
import Text.Parsec.Expr as P.Expr
import Text.ParserCombinators.Parsec.Number as P.Number (floating2)
import qualified Text.PrettyPrint.Leijen as PP

import Nummy.Parser.Base
import Nummy.Parser.Unit
import Nummy.Metrology as M hiding (length)


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
  return $ \q -> (mkQu (-1) dimlessUnit) %* q

quOpIn :: Parser (Quantity -> Quantity)
quOpIn = do
  _ <- spaces
  _ <- string "in" <|> string "into" <|> string "of"
  _ <- space >> spaces
  u <- unit
  guardDim (dimOfUnit u) <?> "Specifier must be of same dimension"
  return $ \q -> fromJust (quIn q u)


quOpTable :: OpTable Quantity
quOpTable =
  [ [ Prefix (try quOpNegate) ]
  , [ Infix (try quOpMult) AssocLeft ]
  , [ Infix (try quOpDiv) AssocLeft ]
  , [ Infix (try quOpSum) AssocLeft ,  Infix (try quOpDif) AssocLeft ]
  , [ Postfix (try quOpIn) ]
  ]

-- Quantity parsers

quantity :: Parser Quantity
quantity = try wideQu <|> try slimQu <|> try dimlessQu <?> "quantity" where
  wideQu = do
    v <- try modifiedValue <|> rawValue
    _ <- space
    u <- unit
    guard . not . U.isDimless $ u
    return $ mkQu v u
  slimQu = do
    v <- rawValue
    u <- unit
    return $ mkQu v u
  dimlessQu = do
    v <- try modifiedValue <|> rawValue
    return $ mkQu v dimlessUnit


parseQuantity :: Parser Quantity
parseQuantity = do
  q <- quantity
  putDim $ dimOfQu q
  return q

expression :: Parser Quantity
expression = buildExpressionParser quOpTable parseQuantity
