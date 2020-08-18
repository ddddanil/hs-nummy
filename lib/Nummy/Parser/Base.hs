{-# LANGUAGE TemplateHaskell #-}

module Nummy.Parser.Base (
  Parser
, ParserState
, ParserError
, ParserResult (..)
, ParserOptions (..), lookupUnit, lookupCmd
, pValue
, parenthesis
) where

import Nummy.Prelude hiding (many, try)
import Control.Lens
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal, float)
import Nummy.Base
import Nummy.Metrology (Unit)

type ParserState cmd = Reader (ParserOptions cmd)

data ParserOptions cmd = ParserOptions
  { _lookupUnit :: Text -> Maybe Unit
  , _lookupCmd :: Text -> Maybe cmd
  }
makeLenses ''ParserOptions

-- | Parser type on 'Text'. The internal monad gives us access to the currency cache and IO operations
type Parser cmd = ParsecT Void Text (ParserState cmd)

-- | Synonym for Megaparsec error type
type ParserError = ParseErrorBundle Text Void

-- | Possible results of parsing
data ParserResult cmd
  = PResult DocN
  | PCommand cmd
  | PError ParserError
  deriving Show

pValue :: Parser c Value
pValue = valueF <$> try float <|> valueI <$> decimal <?> "value"

parenthesis :: Parser c a -> Parser c a
parenthesis = between (char '(' >> space) (char ')' >> space)

