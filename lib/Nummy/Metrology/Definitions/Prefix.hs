{-# OPTIONS_HADDOCK hide #-}
module Nummy.Metrology.Definitions.Prefix where

import Protolude ((*), (/), (+), (-), Integer, (^^), ($))
import Nummy.Metrology.Base

exp :: Integer -> Value
exp e = 10 ^^ e

-- *** Above one

yotta = Prefix (exp 24, "Y")

zetta = Prefix (exp 21, "Z")

exa   = Prefix (exp 18, "E")

peta  = Prefix (exp 15, "P")

tera  = Prefix (exp 12, "T")

giga  = Prefix (exp 9,  "G")

mega  = Prefix (exp 6,  "M")

kilo  = Prefix (exp 3 , "k")

hecto = Prefix (exp 2,  "h")

deca  = Prefix (exp 1, "da")

-- *** Below one

deci  = Prefix (exp (-1), "d")

centi = Prefix (exp (-2), "c")

milli = Prefix (exp (-3), "m")

micro = Prefix (exp (-6), "μ")

nano  = Prefix (exp (-9), "n")

pico  = Prefix (exp (-12), "p")

femto = Prefix (exp (-15), "f")

atto  = Prefix (exp (-18), "a")

zepto = Prefix (exp (-21), "z")

yocto = Prefix (exp (-24), "y")
