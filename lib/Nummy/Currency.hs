module Nummy.Currency (
  getCurrency
) where

import Nummy.Prelude
import Data.Cache
import Data.Aeson
import qualified Data.HashMap.Strict as M
import Network.HTTP.Req

import Nummy.Base (valueF)
import Nummy.Cache
import Nummy.Metrology.Definitions.Dimension (currency)
import Nummy.Metrology.Unit (Unit, conversion_ratio)

data ExchangeResponse =
  ExchangeResponse { base :: Text
                   , rates :: [(Text, Double)]
                   }

instance FromJSON ExchangeResponse where
  parseJSON = withObject "ExchangeResponse" $ \o ->
    ExchangeResponse
      <$> o .: "base"
      <*> (M.toList <$> o .: "rates")

decodeResponse :: ExchangeResponse -> [(Text, Unit)]
decodeResponse r = map make_unit units
  where
    make_unit (s, v) = (s, conversion_ratio currency s (1 / valueF v) )
    units = ((base r), 1) : (rates r)


requestCurrencies :: IO [(Text, Unit)]
requestCurrencies =
  handle ((\_ -> return []) :: HttpException -> IO [(Text, Unit)]) $
  -- Default to an empty list on any exception
  runReq defaultHttpConfig $ do
  r <-
    req
      GET
      (https "api.exchangeratesapi.io" /: "latest")
      NoReqBody
      jsonResponse
      mempty
    :: Req (JsonResponse ExchangeResponse)
  return . decodeResponse . responseBody $ r


getCurrency :: ReadCache [(Text, Unit)]
getCurrency = do
  cache <- ask
  lift $ fetchWithCache cache 0 (const requestCurrencies)

