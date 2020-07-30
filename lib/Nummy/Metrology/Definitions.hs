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
  unitTable, prefixTable, comboTable
-- ** Lookups
, lookupUnit, lookupPrefix
-- ** Dimensions
, module D.D
-- ** Units
, module D.U
-- ** Prefixes
, module D.P
) where

import Protolude hiding (Prefix)
import Data.String (String)
import Data.List as L(lookup)

import Nummy.Metrology.Base as B
import Nummy.Metrology.Dimension as D
import Nummy.Metrology.Unit as U
import Nummy.Metrology.Definitions.Tables as D.T
import qualified Nummy.Metrology.Definitions.Dimension as D.D
import qualified Nummy.Metrology.Definitions.Unit as D.U
import qualified Nummy.Metrology.Definitions.Prefix as D.P


expandSynonyms :: [([Label], a)] -> [(Label, a)]
expandSynonyms xs = concatMap (\(ls, x) -> [ (l, x) | l <- ls] ) xs

-- | All unit synonyms
unitTable :: [(Label, Unit)]
unitTable = sortBy (flip compare `on` length . fst) . expandSynonyms $ unit_table

-- | All prefix synonyms
prefixTable :: [(Label, Prefix)]
prefixTable = sortBy (flip compare `on` length . fst) . expandSynonyms $ prefix_table

-- | All combinations of prefixes and units
comboTable :: [(Label, Unit)]
comboTable = sortBy (flip compare `on` length . fst) $ unitTable ++ (map (uncurry bimap) (bimap (++) (-|) <$> prefixTable) <*> unitTable)

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

-- | Find a prefix
--
-- >>> lookupPrefix "k"
-- Just kilo
-- >>> lookupPrefix "x"
-- Nothing
lookupPrefix :: Label -> Maybe Prefix
lookupPrefix p = snd <$> find (elem p . fst) prefix_table
