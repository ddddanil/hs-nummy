{-# LANGUAGE FlexibleContexts #-}
module Nummy.Parser (quantityT) where

import GHC.Base (String)
import Protolude
import Data.Metrology.Poly ( quOf )
import Data.Metrology.Parser (parseUnitType)
import Data.Metrology.Vector ( MkQu_D, Dimension )
import Language.Haskell.TH as TH
import Text.Parsec as P hiding ( (<|>) )
import Text.ParserCombinators.Parsec.Char as P.Char
import Text.ParserCombinators.Parsec.Prim as P.Prim hiding ( (<|>) )
import Text.ParserCombinators.Parsec.Combinator as P.Comb
import Text.ParserCombinators.Parsec.Number as P.Number (floating2)
import Nummy.Parser.Definitions


-- TH functions

parseUnit :: String -> Either String TH.Type
parseUnit = parseUnitType symbolTable

-- Parsec functions

unitS :: CharParser st String
unitS = many1 unitSymbols <|> parenthesis (unitSymbols `sepBy` P.optional space) where
   unitSymbols = alphaNum <|> oneOf "*/^-"
   parenthesis = between (char '(') (char ')')

unit :: CharParser st TH.Type
unit = do
   u <- unitS
   either unexpected return $ parseUnit u


quantityT :: CharParser st (Double, TH.Type)
quantityT = do
   n <- floating2 True
   _ <- P.optional space
   u <- unit
   return (n, u)
