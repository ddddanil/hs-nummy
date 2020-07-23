module Main where

import Protolude
import Data.String ( fromString )
import Data.Text (unpack, pack)
-- import Data.Metrology.Poly ( quOf )
-- import Data.Metrology.Vector ( MkQu_D, Dimension )
import Text.Parsec (parse)

import Nummy.Parser (parse_nummy)
import Main.Repl (repl)

repackParse = bimap show (pack . show) . parse parse_nummy "" . unpack

main :: IO ()
main = repl repackParse
