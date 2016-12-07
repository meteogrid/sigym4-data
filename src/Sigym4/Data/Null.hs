{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module Sigym4.Data.Null (
  HasNull (..)
) where

import           Data.Functor.Identity

-- | A type (a newtype is highly recommended) that is
-- inhabited by a value which represents the absence
-- of a value.
--
-- Instances must satisfy
--
--   >>> isNull nullValue
--   True
class HasNull a where
  nullValue :: a
  default nullValue :: Bounded a => a
  nullValue = minBound

  isNull    :: a -> Bool
  default isNull :: (Eq a, Bounded a) => a -> Bool
  isNull = (==minBound)

instance HasNull a => HasNull (Identity a) where
  nullValue = pure nullValue
  isNull    = isNull . runIdentity

instance HasNull () where
  nullValue = ()
  isNull () = True
