{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Nummy.Parser where

import GHC.Base (String)
import Protolude
import Data.Metrology.Poly ( (%) )
import Data.Metrology.Parser (parseUnitType, SymbolTable, mkSymbolTable)
-- import Data.Metrology.Qu ( MkQu_U )
import Data.Units.SI
import Data.Units.SI.Prefixes
import Data.Metrology.Show
import Language.Haskell.TH as TH
import Data.Generics
import Text.Parsec as P hiding ( (<|>) )
import Text.ParserCombinators.Parsec.Char as P.Char
import Text.ParserCombinators.Parsec.Prim as P.Prim hiding ( (<|>) )
import Text.ParserCombinators.Parsec.Combinator as P.Comb
import Text.ParserCombinators.Parsec.Number as P.Number (floating2)


-- TH functions

stripModules :: Data a => a -> a
stripModules = everywhere (mkT (mkName . nameBase))

symbolTable :: SymbolTable Name Name
Right symbolTable =
   mkSymbolTable (stripModules [ ("k", ''Kilo)
                               , ("da", ''Deca)
                               , ("m", ''Milli)
                               , ("d", ''Deci) ])
                 (stripModules [ ("m", ''Meter)
                               , ("s", ''Second)
                               , ("min", ''Minute)
                               , ("am", ''Ampere) ])

parseUnit :: String -> Either String TH.Type
parseUnit = parseUnitType symbolTable

-- Parsec functions

unitS :: CharParser st String
unitS = many1 unitSymbols <|> (parenthesis $ unitSymbols `sepBy` P.optional space) where
   unitSymbols = alphaNum <|> oneOf "*/^-"
   parenthesis = between (char '(') (char ')')

unit :: CharParser st TH.Type
unit = do
   u <- unitS
   either unexpected return $ parseUnit u


quantity :: CharParser st (Double, TH.Type)
quantity = do
   n <- floating2 True
   _ <- P.optional space
   u <- unit
   return (n, u)
