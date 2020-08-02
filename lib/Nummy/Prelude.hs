{-# OPTIONS_HADDOCK hide #-}
module Nummy.Prelude (
  module Protolude
, String
, module Data.Tuple.Extra
, module Data.List
, concatMaybe
, (&|&)
) where

import Protolude
import Data.String (String)
import Data.Tuple.Extra hiding (first, second)
import Data.List (lookup, partition, foldl1)


-- | Concat nested maybes
concatMaybe :: Maybe (Maybe a) -> Maybe a
concatMaybe (Just (Just x)) = Just x
concatMaybe _ = Nothing

-- | Boolean XOR
infixr 2 &|&
(&|&) :: Bool -> Bool -> Bool
(&|&) True True = False
(&|&) False False = False
(&|&) _ _ = True
