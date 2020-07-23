module Main where

import Protolude
import Data.String ( fromString )
import Data.Text (unpack, pack)
-- import Data.Metrology.Poly ( quOf )
-- import Data.Metrology.Vector ( MkQu_D, Dimension )
import Text.Parsec (parse)

import Nummy.Parser (parse_nummy)
import Nummy.Metrology.Show (showQu)
import Main.Repl (repl)

smartRepl = repl $ bimap show (pack . showQu) . parse parse_nummy "" . unpack
dumbRepl = interact $ either (pack . show) (pack . showQu) . parse parse_nummy "" . unpack

main :: IO ()
main = dumbRepl
