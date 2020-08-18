module Nummy.Parser.Command where

import Nummy.Prelude hiding (many, Prefix, try)
import Control.Lens
import Data.Char (isAlphaNum)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Fail

import Nummy.Parser.Base

command :: Parser c c
command = do
  _ <- char ':' <?> "command"
  str <- takeWhile1P (Just "base unit") (\c -> isAlphaNum c || c == '_')
  lookupC <- lift $ asks (^. lookupCmd)
  case lookupC str of
    Just cmd -> return cmd
    Nothing -> fail $ "Unknown command \'" ++ T.unpack str ++ "\'"

