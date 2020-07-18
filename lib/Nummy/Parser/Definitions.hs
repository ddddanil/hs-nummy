{-# LANGUAGE TemplateHaskellQuotes #-}
module Nummy.Parser.Definitions (symbolTable) where

import Protolude
import Data.Metrology.Parser (SymbolTable, mkSymbolTable)
import Data.Units.SI
import Data.Units.SI.Prefixes
import Language.Haskell.TH as TH
import Data.Generics

stripModules :: Data a => a -> a
stripModules = everywhere (mkT (mkName . nameBase))

symbolTable :: SymbolTable Name Name
Right symbolTable =
   mkSymbolTable (stripModules [ ("T", ''Tera)
                               , ("G", ''Giga)
                               , ("M", ''Mega)
                               , ("k", ''Kilo)
                               , ("d", ''Deci)
                               , ("c", ''Centi)
                               , ("m", ''Milli)
                               , ("mu", ''Micro)
                               , ("n", ''Nano)
                               , ("p", ''Pico)
                               ])
                 (stripModules [ ("m", ''Meter)
                               , ("g", ''Gram)
                               , ("K", ''Kelvin)
                               , ("l", ''Liter)
                               , ("mol", ''Mole)
                               , ("s", ''Second)
                               , ("min", ''Minute)
                               , ("h", ''Hour)
                               , ("rad", ''Radian)
                               , ("deg", ''Degree)
                               , ("C", ''Coulomb)
                               , ("A", ''Ampere)
                               , ("V", ''Volt)
                               , ("F", ''Farad)
                               , ("Ohm", ''Ohm)
                               , ("N", ''Newton)
                               , ("Pa", ''Pascal)
                               , ("J", ''Joule)
                               , ("W", ''Watt)
                               ])
