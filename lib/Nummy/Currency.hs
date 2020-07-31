module Nummy.Currency (
  Currency(..)
, CurrencyCache
, getCurrencies
, newCurrencyCache
) where

import Nummy.Prelude
import Control.Monad.IO.Class
import Data.Aeson
import Data.Scientific (toRealFloat)
import qualified Data.HashMap.Strict as M
import Network.HTTP.Req
import Data.Cache
import System.Clock (fromNanoSecs)


data Currency =
  Currency { rate :: Double
           , short_name :: Text
           }
  deriving (Eq, Show)

type CurrencyCache = Cache Int [Currency]

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

getCurrencies' :: IO [Currency]
getCurrencies' = runReq defaultHttpConfig $ do
    r <-
      req
        GET
        (https "api.exchangeratesapi.io" /: "latest")
        NoReqBody
        jsonResponse
        mempty
      :: Req (JsonResponse ExchangeResponse)
    return . decodeResponse . responseBody $ r

getCurrencies :: CurrencyCache -> IO [Currency]
getCurrencies cache = fetchWithCache cache 0 (const getCurrencies')

newCurrencyCache :: IO CurrencyCache
newCurrencyCache = newCache . Just . fromNanoSecs $ 60 * 1000000000 -- 1 minute
