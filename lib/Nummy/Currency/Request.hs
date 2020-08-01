module Nummy.Currency.Request (requestCurrencies) where

import Nummy.Prelude
import Data.Aeson
import Data.Scientific (toRealFloat)
import qualified Data.HashMap.Strict as M
import Network.HTTP.Req

import Nummy.Currency.Base

data ExchangeResponse =
  ExchangeResponse { date :: Text
                   , base :: Text
                   , rates :: [(Text, Double)]
                   }

instance FromJSON ExchangeResponse where
  parseJSON = withObject "ExchangeResponse" $
    \v -> ExchangeResponse
      <$> v .: "date"
      <*> v .: "base"
      <*> ((v .: "rates") >>= withObject "Rates" (\o -> return $ M.toList . M.map to_double $ o))
    where
      to_double (Number x) = toRealFloat x

decodeResponse :: ExchangeResponse -> [Currency]
decodeResponse r = Currency 1 (base r)  : map (uncurry . flip $ Currency) (rates r)

requestCurrencies :: IO [Currency]
requestCurrencies = runReq defaultHttpConfig $ do
    r <-
      req
        GET
        (https "api.exchangeratesapi.io" /: "latest")
        NoReqBody
        jsonResponse
        mempty
      :: Req (JsonResponse ExchangeResponse)
    return . decodeResponse . responseBody $ r
