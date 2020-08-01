{-# LANGUAGE NumericUnderscores #-}
module Nummy.Cache where

import Nummy.Prelude
import Data.Cache
import System.Clock (fromNanoSecs)
import Nummy.Currency.Base

type CurrencyCache = Cache Int [Currency]

newCurrencyCache :: IO CurrencyCache
newCurrencyCache = newCache . Just . fromNanoSecs $ 5 * 60 * 1_000_000_000 -- 5 minutes
