module Nummy.Parser.Unit (unit) where

import Protolude hiding (Prefix, Infix, try)
import Data.String (String)
import Data.Ratio (approxRational)
import Text.Parsec as P hiding ( (<|>) )
import Text.Parsec.Char as P.Char
import Text.Parsec.Expr as P.Expr
import qualified Text.PrettyPrint.Leijen as PP

import Nummy.Parser.Base
import Nummy.Metrology as M
import Nummy.Metrology.Definitions (dimless_unit)


-- Unit operations

unitOpPow :: Parser (Unit -> Unit)
unitOpPow = do
  _ <- char '^'
  p <- parseValue
  return $ \u -> u #^ p

unitOpMult :: Parser (Unit -> Unit -> Unit)
unitOpMult = char '*' >> return (#*)

unitOpDiv :: Parser (Unit -> Unit -> Unit)
unitOpDiv = char '/' >> return (#/)

unitOpInverse :: Parser (Unit -> Unit)
unitOpInverse = do
  _ <- string "1/"
  return $ \u -> (dimless_unit #/ u)

unitOpCombine :: Parser (Unit -> Unit -> Unit)
unitOpCombine = char ' ' >> lookAhead baseUnit >> return (#*)


-- Op Tables

fullUnitOpTable :: OpTable Unit
fullUnitOpTable =
  [ [ Postfix unitOpPow ]
  , [ Infix (try unitOpCombine) AssocLeft ]
  , [ Infix unitOpDiv AssocLeft ]
  , [ Infix unitOpMult AssocLeft ]
  , [ Prefix unitOpInverse ]
  ]

shortUnitOpTable :: OpTable Unit
shortUnitOpTable =
  [ [ Postfix unitOpPow ]
  , [ Infix unitOpMult AssocLeft ]
  , [ Infix unitOpDiv AssocLeft ]
  , [ Prefix unitOpInverse ]
  ]


-- Unit parsers

unitExpr :: OpTable Unit -> Parser Unit
unitExpr table = buildExpressionParser table baseUnit

baseUnit :: Parser Unit
baseUnit = try prefixed <|> try parseBaseUnit <?> "base unit" where
  prefixed = do
    p <- parsePrefix
    u <- parseBaseUnit
    return $ p -| u

shortUnit :: Parser Unit
shortUnit = unitExpr shortUnitOpTable

longUnit :: Parser Unit
longUnit = unitExpr fullUnitOpTable

-- | Tries a full unit expression inside parenthesis and defaults to a short expression
unit :: Parser Unit
unit = try (parenthesis longUnit) <|> try shortUnit <?> "unit"


