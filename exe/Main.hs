module Main where

import Protolude
import Data.Metrology.Poly ( (%) )
import Data.Metrology.Parser
import Data.Metrology.SI 
import Data.Metrology.Show

val :: Length
val = 5 Data.Metrology.Poly.% Metre

main :: IO ()
main = print val
