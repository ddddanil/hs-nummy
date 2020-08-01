{-|
Module        : Nummy.Metrology.Definitions
Description   : Definitions for dimensions, units and prefixes
Maintainer    : ddddanil@vivaldi.net
Stability     : experimental
-}

module Nummy.Metrology.Definitions (
-- * Definitions
--
-- ** Symbol tables
  baseUnitTable, comboTable
-- -- ** Lookups
-- , lookupUnit, lookupPrefix
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
import Nummy.Metrology.Definitions.Dimension as D.D hiding (dimless)
import Nummy.Metrology.Definitions.Unit as D.U
import Nummy.Metrology.Definitions.Prefix as D.P


-- | All unit synonyms
unitTable :: [(Label, Unit, [PrefixType])]
unitTable = concatMap (\(ls, u, p) -> [ (l, u, p) | l <- ls] ) $ unit_table

-- | All prefix synonyms
prefixTable :: [(Label, Prefix, PrefixType)]
prefixTable = concatMap (\(ls, p, t) -> [ (l, p, t) | l <- ls] ) $ prefix_table

currencyTable :: ReadUnit [(Label, Unit)]
currencyTable = accessCurrency >>= return . map transformCurrency

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

-- | All combinations of prefixes, units and currencies
comboTable :: ReadUnit [(Label, Unit)]
comboTable = do
  curs <- currencyTable
  return $ sortBy (flip compare `on` T.length . fst) (baseUnitTable ++ curs)


-- Lookups

-- | Find a unit
--
-- Specifying a dimension will narrow down the search
--
-- >>> lookupUnit Nothing "m"
-- Just meter
-- >>> lookupUnit (Just time) "m"
-- Just minute
-- >>> lookupUnit Nothing "x"
-- Nothing
{-
lookupUnit :: Maybe Dimension -- ^ Optional dimension specifier
           -> Label           -- ^ Unit synonym
           -> Maybe Unit      -- ^ Result
lookupUnit md unit =
  let has_unit = filter matches_unit unit_table
  in case md of
    Just dim -> snd <$> find (matches_dimension dim) has_unit
    Nothing -> snd <$> headMay has_unit
  where
    matches_unit = elem unit . fst
    matches_dimension dim = (==dim) . dimension . snd
-}
