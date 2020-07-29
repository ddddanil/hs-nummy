module Nummy.Parser.Base (
  Parser, OpTable
, putDim, guardDim
, oneOfStr, parenthesis
, parseMaybe
, parseBaseUnit, parseModifier, parsePrefix
, rawValue, modifiedValue
) where

import Protolude hiding (Prefix, Infix, try)
import Data.String (String)
import Data.Maybe (fromJust)
import Data.Ratio (approxRational)
import Text.Parsec as P hiding ( (<|>) )
import Text.Parsec.Char as P.Char
import Text.Parsec.Expr as P.Expr
import Text.ParserCombinators.Parsec.Number as P.Number (floating2)

import Nummy.Metrology as M hiding (length)

type Parser = Parsec String (Maybe Dimension)
type OpTable a = OperatorTable String (Maybe Dimension) Identity a


-- State manipulators

putDim :: Dimension -> Parser ()
putDim d = putState $ Just d

guardDim :: Dimension -> Parser ()
guardDim d = do
  curr <- getState
  guard (curr == Just d)


-- Combinators

oneOfStr :: [[Char]] -> Parser [Char]
oneOfStr ss = choice $ map (try . string) . sortBy (flip compare `on` length) $ ss

parseMaybe :: Maybe a -> Parser a
parseMaybe Nothing = parserFail "Could not unpack Maybe"
parseMaybe (Just x) = return x

parenthesis :: Parser a -> Parser a
parenthesis = between (char '(' >> spaces >> notFollowedBy space) (spaces >> char ')')


-- Parsers

parseBaseUnit :: Parser Unit
parseBaseUnit = (oneOfStr baseUnitTable <?> "known unit symbol") >>= \u -> parseMaybe (lookupUnit Nothing u) <?> "known unit symbol"

parsePrefix :: Parser Prefix
parsePrefix = (oneOfStr prefixTable <?> "known prefix") >>= \p -> parseMaybe (lookupPrefix p) <?> "known prefix"

parseModifier :: Parser Modifier
parseModifier = (oneOfStr modifierTable <?> "known modifier") >>= \m -> parseMaybe (lookupModifier m) <?> "known modifier"

modifiedValue :: Parser Value
modifiedValue = do
  n <- rawValue
  m <- parseModifier
  return $ applyModifier m n

rawValue :: Parser Value
rawValue = do
  n <- floating2 False :: Parser Double
  return $ valueF n

