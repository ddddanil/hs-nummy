module Nummy.Parser.Base (
  Parser
, ParserError
, ParserResult (..)
, pValue
, parenthesis
) where

import Nummy.Prelude hiding (many, try)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal, float)
import Nummy.Base
import Nummy.Cache

-- | Parser type on 'Text'. The internal monad gives us access to the currency cache and IO operations
type Parser = ParsecT Void Text ReadCache

-- | Synonym for Megaparsec error type
type ParserError = ParseErrorBundle Text Void

-- | Possible results of parsing
data ParserResult
  = PResult DocN
  | PError ParserError
  deriving Show

pValue :: Parser Value
pValue = valueF <$> try float <|> valueI <$> decimal <?> "value"

parenthesis :: Parser a -> Parser a
parenthesis = between (char '(' >> space) (char ')' >> space)

