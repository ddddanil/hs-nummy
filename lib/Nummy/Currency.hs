module Nummy.Currency (
  Currency(..)
, getCurrencies
) where

import Nummy.Prelude
import Data.Cache
import Nummy.Cache
import Nummy.Currency.Base
import Nummy.Currency.Request

getCurrencies :: CurrencyCache -> IO [Currency]
getCurrencies cache = fetchWithCache cache 0 (const requestCurrencies)

