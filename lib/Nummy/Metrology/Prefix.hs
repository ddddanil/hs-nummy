module Nummy.Metrology.Prefix (
  Prefix(..), PrefixType(..)
) where

import Nummy.Prelude hiding (Prefix)
import Data.Text.Prettyprint.Doc

import Nummy.Base


-- Prefix

-- | Representation of a unit prefix
newtype Prefix = Prefix (Value, Text) -- ^ (prefix multiplier, prefix name)
  deriving (Show, Eq)

data PrefixType = PrefixBelowOne | PrefixBinary | PrefixAboveOne
  deriving (Eq, Show, Ord, Enum)

instance Pretty Prefix where
  pretty (Prefix (_, l)) = pretty l

