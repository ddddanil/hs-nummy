module Nummy.Metrology.Show (showDim, showQu) where

import Protolude hiding (unwords)
import Data.String (String, unwords)
import Data.List (partition)
import Data.Bifoldable (bifoldl1)

import Nummy.Metrology.Definitions
import Nummy.Metrology.Dimension
import Nummy.Metrology.Unit

showDim :: Dimension -> String
showDim d = bifoldl1 (\a b -> a ++ "/" ++ b) . bimap showLine showLine . partition positive_pow $ d where
  positive_pow = (> 0) . snd
  showLine = unwords . map (\(d, p) -> showBase d ++ if abs p /= 1 then "^" ++ show (fromRational p :: Double) else "")
  showBase "Length" = "m"
  showBase "Mass" = "kg"
  showBase "Time" = "s"

showQu :: Quantity -> String
showQu (d, v) = show (fromRational v :: Double) ++ " " ++ showDim d
