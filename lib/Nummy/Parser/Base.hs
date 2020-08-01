module Nummy.Parser.Base (
  Parser
, pValue
, parenthesis
) where

import Nummy.Prelude hiding (many, try)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal, float)
import Nummy.Metrology

-- | Parser type on 'Label'. The internal monad gives us access to the currency cache and IO operations
type Parser = ParsecT Void Label ReadUnit

pValue :: Parser Value
pValue = valueF <$> try float <|> valueI <$> decimal <?> "value"

parenthesis :: Parser a -> Parser a
parenthesis = between (char '(' >> space) (char ')' >> space)

