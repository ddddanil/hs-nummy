{-|
Module        : Nummy.Metrology.Definitions
Description   : Definitions for dimensions, units and prefixes
Maintainer    : ddddanil@vivaldi.net
Stability     : experimental
-}

module Nummy.Metrology.Definitions (
-- * Definitions
--
-- ** Lookups
  lookupUnit
, baseUnitTable    -- Uncomment for inspection/debug
-- ** Dimensions
, module D.D
-- ** Units
, module D.U
-- ** Prefixes
, module D.P
) where

import Nummy.Prelude hiding (Prefix)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Char (isUpper)
import qualified Data.Text as T

import Nummy.Currency
import Nummy.Metrology.Dimension as D
import Nummy.Metrology.Unit as U
import Nummy.Metrology.Prefix as P
import Nummy.Metrology.Definitions.Dimension as D.D
import Nummy.Metrology.Definitions.Unit as D.U
import Nummy.Metrology.Definitions.Prefix as D.P
import Nummy.Metrology.Definitions.Tables as D.T
import Nummy.Cache


-- | All unit synonyms
unitTable :: [(Text, Unit, [PrefixType])]
unitTable = concatMap (\(ls, u, p) -> [ (l, u, p) | l <- ls] ) $ unit_table

-- | All prefix synonyms
prefixTable :: [(Text, Prefix, PrefixType)]
prefixTable = concatMap (\(ls, p, t) -> [ (l, p, t) | l <- ls] ) $ prefix_table

-- | All currencies
currencyTable :: ReadCache [(Text, Unit)]
currencyTable = getCurrency

-- | Mix units with allowed prefixes
baseUnitTable :: [(Text, Unit)]
baseUnitTable = sortOn (T.length . fst) $ map (\(a, b, _)->(a, b)) unitTable ++ units_with_prefixes
  where
      units_with_prefixes =
        [ ( T.append pl ul
          , p -| u
          )
        | (pl, p, pt) <- prefixTable,
          (ul, u, ut) <- unitTable,
          pt `elem` ut &&
          not (T.length pl <= 2 &|&      -- Short-hand prefix = k mu da G
               T.length ul <= 2 )        -- Short-hand unit   = m s Pa J
          -- Combine short-hand prefixes with short-hand units and vice versa
        ]


-- Lookups

-- | Find a unit
--
-- In case there are several competing synonyms, one will be chosen according to
-- the implicit preference in the unit table. Specifying a dimension will narrow
-- down the search
--
-- The search prefers statically defined units, and only accesses the currency
-- cache when it can't find the unit in other tables.
--
-- >>> lookupUnit Nothing "m"
-- Just meter
-- >>> lookupUnit (Just time) "m"
-- Just minute
-- >>> lookupUnit Nothing "x"
-- Nothing
lookupUnit :: Maybe Dimension         -- ^ Optional dimension specifier
           -> Text                    -- ^ Unit synonym
           -> MaybeT ReadCache Unit   -- ^ Result
lookupUnit md u =
  liftMaybe munit <|> if isCurrency then (lift  mcurrency >>= liftMaybe) else mzero
  where
    getUnit = fmap snd . headMay
    filterDim = filter (maybe (const True) (==) md . dimension . snd)
    filterUnit = filter ((== u) . fst)
    isCurrency = T.length u == 3 && T.all isUpper u && (md == Nothing || md == Just currency)
    munit = getUnit . filterDim . filterUnit $ baseUnitTable
    mcurrency = getUnit . filterDim . filterUnit <$> currencyTable
