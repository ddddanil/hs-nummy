{-|
Module        : Nummy.Cache
Description   : Caching functionality for expensive unit definitions
Maintainer    : ddddanil@vivaldi.net
Stability     : experimental
-}

module Nummy.Cache where

import Nummy.Prelude
import Data.Cache
import System.Clock (fromNanoSecs)
import Nummy.Metrology.Unit (Unit)


-- | Caches all recorded currencies
type CurrencyCache = Cache Int [(Text, Unit)]

-- | Make a new cache with a 5 minute timeout
newCurrencyCache :: IO CurrencyCache
newCurrencyCache = newCache . Just . fromNanoSecs $ 5 * 60 * 1_000_000_000 -- 5 minutes


-- | Reader monad that gives access to the cache
type ReadCache = ReaderT CurrencyCache IO

-- | Run the Cache Reader monad
runReadCache :: ReaderT CurrencyCache IO a -> IO a
runReadCache m = newCurrencyCache >>= runReaderT m
