{-# OPTIONS_HADDOCK hide #-}
module Nummy.Metrology.Definitions.Prefix where

import Nummy.Prelude ((^^))
import Nummy.Metrology.Prefix


-- *** Above one

-- | 10^24
yotta :: Prefix
yotta = Prefix (10 ^^ 24, "Y")

-- | 10^21
zetta :: Prefix
zetta = Prefix (10 ^^ 21, "Z")

-- | 10^18
exa :: Prefix
exa   = Prefix (10 ^^ 18, "E")

-- | 10^15
peta :: Prefix
peta  = Prefix (10 ^^ 15, "P")

-- | 10^12
tera :: Prefix
tera  = Prefix (10 ^^ 12, "T")

-- | 10^9
giga :: Prefix
giga  = Prefix (10 ^^ 9,  "G")

-- | 10^6
mega :: Prefix
mega  = Prefix (10 ^^ 6,  "M")

-- | 10^3
kilo :: Prefix
kilo  = Prefix (10 ^^ 3 , "k")

-- | 10^2
hecto :: Prefix
hecto = Prefix (10 ^^ 2,  "h")

-- | 10^1
deca :: Prefix
deca  = Prefix (10 ^^ 1, "da")

-- *** Below one

-- | 10^-1
deci :: Prefix
deci  = Prefix (10 ^^ (-1), "d")

-- | 10^-2
centi :: Prefix
centi = Prefix (10 ^^ (-2), "c")

-- | 10^-3
milli :: Prefix
milli = Prefix (10 ^^ (-3), "m")

-- | 10^-6
micro :: Prefix
micro = Prefix (10 ^^ (-6), "μ")

-- | 10^-9
nano :: Prefix
nano  = Prefix (10 ^^ (-9), "n")

-- | 10^-12
pico :: Prefix
pico  = Prefix (10 ^^ (-12), "p")

-- | 10^-15
femto :: Prefix
femto = Prefix (10 ^^ (-15), "f")

-- | 10^-18
atto :: Prefix
atto  = Prefix (10 ^^ (-18), "a")

-- | 10^-21
zepto :: Prefix
zepto = Prefix (10 ^^ (-21), "z")

-- | 10^-24
yocto :: Prefix
yocto = Prefix (10 ^^ (-24), "y")


-- *** Binary

-- | [wiki](https://en.wikipedia.org/wiki/Binary_prefix)

-- | 2^10
kibi :: Prefix
kibi = Prefix(2 ^^ 10, "Ki")

-- | 2^20
mebi :: Prefix
mebi = Prefix(2 ^^ 20, "Mi")

-- | 2^30
gibi :: Prefix
gibi = Prefix(2 ^^ 30, "Gi")

-- | 2^40
tebi :: Prefix
tebi = Prefix(2 ^^ 40, "Ti")

-- | 2^50
pebi :: Prefix
pebi = Prefix(2 ^^ 50, "Pi")

-- | 2^60
exbi :: Prefix
exbi = Prefix(2 ^^ 60, "Ei")

-- | 2^70
zebi :: Prefix
zebi = Prefix(2 ^^ 70, "Zi")

-- | 2^80
yobi :: Prefix
yobi = Prefix(2 ^^ 80, "Yi")
