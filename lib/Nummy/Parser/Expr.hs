module Nummy.Parser.Expr (
  quantity
, expression
, line
) where

import Nummy.Prelude hiding (many, Prefix, try)
import Data.Bifoldable (bimsum)
import Data.Maybe (fromJust)
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Fail
import Control.Monad.Combinators.Expr

import Nummy.Parser.Base
import Nummy.Parser.Unit
import Nummy.Metrology
import Nummy.Metrology.Definitions (comboTable)
import Nummy.Metrology.Definitions.Unit (dimless)


-- Term

quantity :: Parser Quantity
quantity = do
  v <- pValue
  u <- optional . try $ do
    _ <- optional spaceChar
    parenthesis unit <|> unit
  _ <- space
  return $ maybe (v %# dimless) ((%#) v) u


-- Operators

pOpQuPow :: Parser (Maybe Quantity -> Maybe Quantity)
pOpQuPow = do
  _ <- char '^'
  _ <- space
  p <- pValue
  return $ fmap (%^ p)

pOpQuNeg :: Parser (Maybe Quantity -> Maybe Quantity)
pOpQuNeg = do
  _ <- char '-'
  return $ fmap (%* (-1) %# dimless)

pOpQuMul :: Parser (Maybe Quantity -> Maybe Quantity -> Maybe Quantity)
pOpQuMul = do
  _ <- char '*'
  _ <- space
  return $ \a b -> (%*) <$> a <*> b

pOpQuDiv :: Parser (Maybe Quantity -> Maybe Quantity -> Maybe Quantity)
pOpQuDiv = do
  _ <- char '/'
  _ <- space
  return $ \a b -> (%/) <$> a <*> b

pOpQuAdd :: Parser (Maybe Quantity -> Maybe Quantity -> Maybe Quantity)
pOpQuAdd = do
  _ <- char '+'
  _ <- space
  return $ \a b -> concatMaybe $ (%+) <$> a <*> b

pOpQuSub :: Parser (Maybe Quantity -> Maybe Quantity -> Maybe Quantity)
pOpQuSub = do
  _ <- char '-'
  _ <- space
  return $ \a b -> concatMaybe $ (%-) <$> a <*> b


-- Operator table

opQuTable :: [[Operator Parser (Maybe Quantity)]]
opQuTable =
  [ [ Prefix  pOpQuNeg ]
  , [ Postfix pOpQuPow ]
  , [ InfixL  pOpQuMul ]
  , [ InfixL  pOpQuDiv ]
  , [ InfixL  pOpQuSub, InfixL pOpQuAdd ]
  ]


-- Expression builder

expression :: Parser (Maybe Quantity)
expression = makeExprParser (Just <$> quantity) opQuTable


-- Line parser

pFormat :: Parser (Quantity -> Maybe Quantity)
pFormat = do
  _ <- char '|'
  _ <- space
  u <- unit
  return $ (%<| u)


line :: Parser Quantity
line = do
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
