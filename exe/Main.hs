module Main where

import Protolude
import Data.String ( fromString )
import Data.Text (unpack, pack)
-- import Data.Metrology.Poly ( quOf )
-- import Data.Metrology.Vector ( MkQu_D, Dimension )
import Text.Parsec (parse)

import Nummy.Parser (parse_nummy)
import Main.Repl (repl)

main :: IO ()
main = repl $ parse parse_nummy ""
