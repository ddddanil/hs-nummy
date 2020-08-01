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
-- ** Dimensions
, module D.D
-- ** Units
, module D.U
-- ** Prefixes
, module D.P
) where

import Nummy.Prelude hiding (Prefix)
import qualified Data.Text as T

import Nummy.Metrology.Base as B
import Nummy.Metrology.Dimension as D
import Nummy.Metrology.Unit as U
import Nummy.Metrology.Definitions.Tables as D.T
import Nummy.Metrology.Currency
import Nummy.Metrology.Definitions.Dimension as D.D
import Nummy.Metrology.Definitions.Unit as D.U
import Nummy.Metrology.Definitions.Prefix as D.P
import Nummy.Cache


-- | All unit synonyms
unitTable :: [(Label, Unit, [PrefixType])]
unitTable = concatMap (\(ls, u, p) -> [ (l, u, p) | l <- ls] ) $ unit_table

-- | All prefix synonyms
prefixTable :: [(Label, Prefix, PrefixType)]
prefixTable = concatMap (\(ls, p, t) -> [ (l, p, t) | l <- ls] ) $ prefix_table

-- | All currencies
currencyTable :: ReadCache [(Label, Unit)]
currencyTable = accessCurrency >>= return . map transformCurrency

-- | Mix units with allowed prefixes
baseUnitTable :: [(Label, Unit)]
baseUnitTable = map (\(a, b, _)->(a, b)) unitTable ++ units_with_prefixes
  where
      units_with_prefixes =
        [ ( T.append pl ul
          , p -| u
          )
        | (pl, p, pt) <- prefixTable,
          (ul, u, ut) <- unitTable,
          pt `elem` ut
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
lookupUnit :: Maybe Dimension          -- ^ Optional dimension specifier
           -> Label                    -- ^ Unit synonym
           -> ReadCache (Maybe Unit)   -- ^ Result
lookupUnit md unit =
  case find_unit baseUnitTable of
    Just u -> return . return $ u
    Nothing -> find_unit <$> currencyTable
  where
    find_unit :: [(Label, Unit)] -> Maybe Unit
    find_unit t =
      let has_unit = filter matches_unit t
      in case md of
        Just dim -> snd <$> find (matches_dimension dim) has_unit
        Nothing ->  snd <$> headMay has_unit
    matches_unit = (== unit) . fst
    matches_dimension dim = (==dim) . dimension . snd
