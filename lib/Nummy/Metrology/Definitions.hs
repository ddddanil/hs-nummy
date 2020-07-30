module Nummy.Metrology.Definitions (
-- * Definitions
--
-- *** Symbol tables
  unitTable, prefixTable
-- *** Lookups
, lookupUnit, lookupPrefix
-- *** Dimensions
, module Nummy.Metrology.Definitions.Dimension
-- *** Units
, module Nummy.Metrology.Definitions.Unit
, dimless_unit
-- *** Prefixes
, module Nummy.Metrology.Definitions.Prefix
) where

import Protolude hiding (length, Prefix)
import Data.String (String)
import Data.List (lookup)

import Nummy.Metrology.Base
import Nummy.Metrology.Dimension
import Nummy.Metrology.Unit
import Nummy.Metrology.Definitions.Dimension
import Nummy.Metrology.Definitions.Unit
import Nummy.Metrology.Definitions.Prefix hiding (exp)
import Nummy.Metrology.Definitions.Tables


-- | All unit synonyms
unitTable :: [Label]
unitTable = concat . map fst $ unit_table

-- | All prefix synonyms
prefixTable :: [Label]
prefixTable = concat . map fst $ prefix_table


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
