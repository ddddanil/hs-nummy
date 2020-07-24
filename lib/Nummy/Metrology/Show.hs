module Nummy.Metrology.Show (showDim, showQu) where

import Prelude hiding (unwords)
import Data.String (String, unwords)
import Data.List (partition)
import Data.Bifoldable (bifoldl1)
import Data.Bifunctor (bimap)

import Nummy.Metrology.Definitions
import Nummy.Metrology.Dimension
import Nummy.Metrology.Unit

instance Show BaseDim where
  show Length = "m"
  show Mass = "kg"
  show Time = "s"
  show _ = ""

showDim :: Dimension -> String
showDim dim
  | isDimless dim || isNone dim = ""
  | otherwise = showDim' dim
  where
    showDim' (Dimension d) = combineLines . bimap showLine showLine . partition positive_pow $ d
    positive_pow = (> 0) . snd
    showLine = unwords . map (\(d, p) -> show d ++ if abs p /= 1 then "^" ++ show (fromRational p :: Double) else "")
    combineLines ("", den) = "1/" ++ den
    combineLines (num, "") = num
    combineLines (num, den) = num ++ "/" ++ den

showQu :: Quantity -> String
showQu (d, v) = show (fromRational v :: Double) ++ " " ++ showDim d
