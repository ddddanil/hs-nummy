module Main where

import Protolude hiding (getLine)
import Data.Metrology.Show
import Text.Parsec (parse)
import System.IO (getLine)

import Nummy.Parser (quantity)

main :: IO ()
main = print =<< parse quantity "" <$> getLine
