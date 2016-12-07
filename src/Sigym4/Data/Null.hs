{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Sigym4.Data.Null (
  HasNull (..)
) where

import           Data.Functor.Identity
import           Numeric.IEEE (IEEE(..))

class HasNull a where
  nullValue :: a
  default nullValue :: IEEE a => a
  nullValue = nan

  isNull    :: a -> Bool
  default isNull :: IEEE a => a -> Bool
  isNull = identicalIEEE nan

instance HasNull a => HasNull (Identity a) where
  nullValue = pure nullValue
  isNull    = isNull . runIdentity

-- FIXME: This instance should be submitted upstream to Numeric.IEEE
instance IEEE a => IEEE (Identity a) where
  infinity     = pure infinity
  minNormal    = pure minNormal
  maxFinite    = pure maxFinite
  epsilon      = pure epsilon
  copySign a b = pure (copySign (runIdentity a) (runIdentity b))
  identicalIEEE a b = identicalIEEE (runIdentity a) (runIdentity b)
  succIEEE     = fmap succIEEE
  predIEEE     = fmap predIEEE
  bisectIEEE a b = pure (bisectIEEE (runIdentity a) (runIdentity b))
  sameSignificandBits a b = sameSignificandBits (runIdentity a) (runIdentity b)
  nan          = pure nan
  nanWithPayload = pure . nanWithPayload
  maxNaNPayload = maxNaNPayload . runIdentity
  nanPayload = nanPayload . runIdentity

