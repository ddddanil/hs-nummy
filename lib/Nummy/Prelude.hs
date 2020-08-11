{-# OPTIONS_HADDOCK hide #-}
module Nummy.Prelude (
  module Protolude
, String
, module Data.Tuple.Extra
, module Data.List
, (&|&)
, whenJust
, liftMaybe
) where

import Protolude
import Data.String (String)
import Data.Tuple.Extra hiding (first, second)
import Data.List (lookup, partition, foldl1)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))


-- | Boolean XOR
infixr 2 &|&
(&|&) :: Bool -> Bool -> Bool
(&|&) True True = False
(&|&) False False = False
(&|&) _ _ = True

-- | Conditional execution based on a maybe
whenJust :: (Applicative m) => Maybe a -> (a -> m () ) -> m ()
whenJust (Just a) k = k a
whenJust Nothing _ = pure ()

liftMaybe :: (Monad m) => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return
