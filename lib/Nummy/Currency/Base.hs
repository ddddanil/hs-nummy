module Nummy.Currency.Base where

import Nummy.Prelude

data Currency =
  Currency { rate :: Double
           , short_name :: Text
           }
  deriving (Eq, Show)
