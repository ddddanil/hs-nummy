module Nummy.Metrology.Currency (transformCurrency, accessCurrency) where

import Nummy.Prelude
import Nummy.Metrology.Base
import Nummy.Metrology.Definitions.Dimension
import Nummy.Metrology.Unit
import Nummy.Currency

transformCurrency :: Currency -> (Label, Unit)
transformCurrency Currency{ rate = r, short_name = n } =
  (n, conversion_ratio currency n (1 / valueF r) )

accessCurrency :: ReadUnit [Currency]
accessCurrency = ask >>= lift <$> getCurrencies
