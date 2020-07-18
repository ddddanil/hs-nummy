module Main where

import Protolude
import Data.Metrology.Show
import Data.String ( fromString )
import Data.Text (unpack)
-- import Data.Metrology.Poly ( quOf )
-- import Data.Metrology.Vector ( MkQu_D, Dimension )
import Text.Parsec (parse)

import Main.Repl (repl)
import Nummy.Parser (quantityT)

showTuple (n, u) = show n ++ " " ++ show u
action = parse quantityT ""
showParse s = second (fromString . showTuple) $ action (unpack s)

main :: IO ()
main = repl showParse
