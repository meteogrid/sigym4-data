{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}

module Sigym4.Data.Maybe (
    Maybe(Nothing)
  , Nullable (..)
  , isNothing
  , isJust
  , fromMaybe
  , fromNullable
  , maybe
  , toNullableVectorWith
  , nullableFromVectorWith
  , nullableFromVector
) where

import Sigym4.Data.Null

import Control.Applicative (Applicative(..), liftA2)
import Control.DeepSeq (NFData(rnf))
import Control.Newtype

import Data.Typeable (Typeable)

import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Base as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Foreign.Ptr (castPtr)
import Foreign.Storable

import Prelude ( Functor(..), Num(..)
               , Fractional(..), Eq(..), Ord(..), Show(..)
               , Bool(..), (.), undefined, not, id
               )

-- | A strict version of Prelude's 'Prelude.Maybe' which has many
-- numeric, 'U.Unbox' and 'Storable' instances implemented for
-- types which implement instances of 'HasNull'.
--
-- 'Just' is not exported so 'Just nullValue' cannot be created.
-- Implement 'HasNull' and use 'fromNullable' instead.
data Maybe a
  = Nothing
  | Just !a
  deriving (Eq, Ord, Show, Typeable, Functor)

newtype Nullable a = Nullable { unNullable :: Maybe a}
  deriving (Eq, Ord, Show, Typeable, Functor, Applicative, Num, Fractional, NFData)

instance Newtype (Nullable a) (Maybe a) where
  pack   = Nullable
  {-# INLINE pack #-}
  unpack = unNullable
  {-# INLINE unpack #-}

instance Newtype (Maybe a) (Maybe a) where
  pack   = id
  {-# INLINE pack #-}
  unpack = id
  {-# INLINE unpack #-}

instance Applicative Maybe where
  pure = Just
  {-# INLINE pure #-}
  Nothing  <*> _m = Nothing
  Just v   <*>  m = fmap v m
  {-# INLINE (<*>) #-}
  Nothing   *> _m2 = Nothing
  Just _    *>  m2 = m2
  {-# INLINE (*>) #-}

instance Num a => Num (Maybe a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = Just . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional a => Fractional (Maybe a) where
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  recip = fmap recip
  {-# INLINE recip #-}
  fromRational = Just . fromRational
  {-# INLINE fromRational #-}


instance NFData a => NFData (Maybe a) where
  rnf (Just a )  = rnf a
  rnf Nothing    = ()
  {-# INLINE rnf #-}

instance (Storable a, HasNull a) => Storable (Nullable a) where
  sizeOf _    = sizeOf (undefined :: a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined :: a)
  {-# INLINE alignment #-}
  peek = fmap fromNullable . peek . castPtr
  {-# INLINE peek #-}
  poke p = poke (castPtr p) . fromMaybe nullValue
  {-# INLINE poke #-}

isNothing :: Newtype t (Maybe a) => t -> Bool
isNothing (unpack -> Nothing)   = True
isNothing _                     = False
{-# INLINE isNothing #-}

isJust :: Newtype t (Maybe a) => t -> Bool
isJust = not . isNothing
{-# INLINE isJust #-}

fromMaybe :: Newtype t (Maybe a) => a -> t -> a
fromMaybe _ (unpack -> Just x) = x
fromMaybe x _                  = x
{-# INLINE fromMaybe #-}

maybe :: Newtype t (Maybe a) => b -> (a -> b) -> t -> b
maybe _ f (unpack -> Just x) = f x
maybe x _ _                  = x
{-# INLINE maybe #-}

-- | Create a 'Maybe' value from a value of the underlying type
-- which must implement 'HasNull'
--
-- If the underlying value satisfies 'isNull' then a 'Nothing'
-- will be created, else a 'Just'.
fromNullable :: (Newtype t (Maybe a), HasNull a) => a -> t
fromNullable v = pack (if isNull v then Nothing else Just v)
{-# INLINE fromNullable #-}

newtype instance U.Vector    (Nullable a) = V_Nullable  { unVN  :: U.Vector a }
newtype instance U.MVector s (Nullable a) = MV_Nullable { unVNM :: UM.MVector s a }
instance (HasNull a, U.Unbox a) => U.Unbox (Nullable a)


instance (M.MVector UM.MVector a, HasNull a) => M.MVector U.MVector (Nullable a) where
  basicLength = M.basicLength . unVNM
  {-# INLINE basicLength #-}
  basicUnsafeSlice m n = MV_Nullable . M.basicUnsafeSlice m n . unVNM
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps v = M.basicOverlaps (unVNM v) . unVNM
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew = fmap MV_Nullable . M.basicUnsafeNew
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeRead v = fmap fromNullable . M.basicUnsafeRead (unVNM v)
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite v i = M.basicUnsafeWrite (unVNM v) i . fromMaybe nullValue
  {-# INLINE basicUnsafeWrite #-}
#if MIN_VERSION_vector(0,11,0)
  basicInitialize = M.basicInitialize . unVNM
  {-# INLINE basicInitialize #-}
#endif

instance (G.Vector U.Vector a, HasNull a) => G.Vector U.Vector (Nullable a) where
  basicUnsafeFreeze = fmap V_Nullable . G.basicUnsafeFreeze . unVNM
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw = fmap MV_Nullable . G.basicUnsafeThaw . unVN
  {-# INLINE basicUnsafeThaw #-}
  basicLength = G.basicLength . unVN
  {-# INLINE basicLength #-}
  basicUnsafeSlice m n = V_Nullable . G.basicUnsafeSlice m n . unVN
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM v = fmap fromNullable . G.basicUnsafeIndexM (unVN v)
  {-# INLINE basicUnsafeIndexM #-}

-- | Create a vector of 'Nullables's from a vector of the underlying
-- type which represents the absence of a value with the same
-- 'nullValue' as defined in the type's 'HasNull' instance.
--
-- This *should not* be used to interface with external sources
-- (eg: 'Storable' vectors from ffi) which might
-- represent the null value with a different 'nullValue'. Use
-- 'nullableFromVectorWith' instead.
nullableFromVector
  :: (G.Vector v a, G.Vector v (Nullable a), HasNull a)
  => v a -> v (Nullable a)
nullableFromVector = G.map fromNullable
{-# INLINE nullableFromVector #-}

-- | Create a vector of 'Nullables's from a vector of the underlying
-- type and a given "null" value.
--
-- This can be used to interface with external sources
-- (eg: 'Storable' vectors from ffi) which might
-- represent the null value with a different 'nullValue'.
nullableFromVectorWith
  :: (G.Vector v a, G.Vector v (Nullable a), Eq a)
  => a -> v a -> v (Nullable a)
nullableFromVectorWith nd = G.map (pack . (\v -> if v==nd then Nothing else Just v))
{-# INLINE nullableFromVectorWith #-}

-- | Unwrap a vector of 'Nullable's to a vector of the underlying
-- type with a given "null" value.
--
-- This can be used to interface with external sources
-- (eg: 'Storable' vectors from ffi) which might
-- represent the null value with a different 'nullValue'.
toNullableVectorWith
  :: (G.Vector v a, G.Vector v (Nullable a))
  => a -> v (Nullable a) -> v a
toNullableVectorWith nd = G.map (fromMaybe nd)
{-# INLINE toNullableVectorWith #-}
