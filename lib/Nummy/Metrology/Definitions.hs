{-|
Module        : Nummy.Metrology.Definitions
Description   : Definitions for dimensions, units and prefixes
Maintainer    : ddddanil@vivaldi.net
Stability     : experimental
-}

module Nummy.Metrology.Definitions (
-- * Definitions
--
  unitTable
-- ** Dimensions
, module D.D
-- ** Units
, module D.U
-- ** Prefixes
, module D.P
) where

import Nummy.Prelude hiding (Prefix)
import qualified Data.Text as T

import Nummy.Metrology.Unit as U
import Nummy.Metrology.Prefix as P
import Nummy.Metrology.Definitions.Dimension as D.D
import Nummy.Metrology.Definitions.Unit as D.U
import Nummy.Metrology.Definitions.Prefix as D.P
import Nummy.Metrology.Definitions.Tables as D.T


-- | All unit synonyms
baseUnitTable :: [(Text, Unit, [PrefixType])]
baseUnitTable = concatMap (\(ls, u, p) -> [ (l, u, p) | l <- ls] ) $ unit_table

-- | All prefix synonyms
prefixTable :: [(Text, Prefix, PrefixType)]
prefixTable = concatMap (\(ls, p, t) -> [ (l, p, t) | l <- ls] ) $ prefix_table

-- | Mix units with allowed prefixes
unitTable :: [(Text, Unit)]
unitTable = sortOn (T.length . fst) $ map (\(a, b, _)->(a, b)) baseUnitTable ++ units_with_prefixes
  where
      units_with_prefixes =
        [ ( T.append pl ul
          , p -| u
          )
        | (pl, p, pt) <- prefixTable,
          (ul, u, ut) <- baseUnitTable,
          pt `elem` ut &&
          not (T.length pl <= 2 &|&      -- Short-hand prefix = k mu da G
               T.length ul <= 2 )        -- Short-hand unit   = m s Pa J
          -- Combine short-hand prefixes with short-hand units and vice versa
        ]

