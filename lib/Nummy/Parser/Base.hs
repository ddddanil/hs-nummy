module Nummy.Parser.Base (
  Parser, OpTable
, putDim, guardDim
, oneOfStr, parenthesis
, parseMaybe
, baseUnitParsers
, parseValue
) where

import Protolude hiding (Prefix, Infix, try)
import Data.String (String)
import Data.Maybe (fromJust)
import Data.Ratio (approxRational)
import Text.Parsec as P hiding ( (<|>) )
import Text.Parsec.Char as P.Char
import Text.Parsec.Expr as P.Expr
import Text.ParserCombinators.Parsec.Number as P.Number (floating2)

import Nummy.Metrology as M
import Nummy.Metrology.Definitions (unitTable, prefixTable, comboTable, lookupUnit, lookupPrefix)


-- | Parsec parser operating on 'String's and having an optional 'Dimension' as user state
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
oneOfStr ss = choice $ map (try . string)  $ ss

parseMaybe :: Maybe a -> Parser a
parseMaybe Nothing = parserFail "Could not unpack Maybe"
parseMaybe (Just x) = return x

parenthesis :: Parser a -> Parser a
parenthesis = between (char '(' >> spaces >> notFollowedBy space) (spaces >> char ')')


-- Parsers

baseUnitParsers :: [Parser Unit]
baseUnitParsers = map parserfy comboTable where
  parserfy :: (Label, Unit) -> Parser Unit
  parserfy (l, u) = try $ do
    _ <- string l
    return u

parseValue :: Parser Value
parseValue = do
  n <- floating2 False :: Parser Double
  return $ valueF n

