{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Nummy.Parser (quantityT) where

import GHC.Base (String)
import Protolude
import Text.Parsec as P hiding ( (<|>) )
import Text.ParserCombinators.Parsec.Char as P.Char
import Text.ParserCombinators.Parsec.Prim as P.Prim hiding ( (<|>) )
import Text.ParserCombinators.Parsec.Combinator as P.Comb
import Text.ParserCombinators.Parsec.Number as P.Number (floating2)


-- Parsec functions

unitS :: CharParser st String
unitS = many1 unitSymbols <|> parenthesis (unitSymbols `sepBy` P.optional space) where
   unitSymbols = alphaNum <|> oneOf "*/^-"
   parenthesis = between (char '(') (char ')')

unit :: CharParser st TH.Type
unit = do
   u <- unitS
   either unexpected return $ parseUnit u

