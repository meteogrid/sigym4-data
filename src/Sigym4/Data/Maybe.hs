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
    Maybe(..)
  , Nullable (..)
  , MaskType
  , isNothing
  , isJust
  , fromMaybe
  , fromNullable
  , maybe
  , toNullableVectorWith
  , nullableFromVectorWith
  , nullableFromVector
  , fromMaskAndVector
  , toMaskAndVector
) where

import Sigym4.Data.Null

import Control.Applicative (Applicative(..), (<$>), liftA2)
import Control.DeepSeq (NFData(rnf))
import Control.Newtype

import Data.Typeable (Typeable)

import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Base as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Data.Word

import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable

import Prelude ( Functor(..), Num(..)
               , Fractional(..), Eq(..), Ord(..), Show(..), Read (..)
               , Bool(..), (.), return, undefined, not, id, (>>), ($!)
               )

-- | A strict version of Prelude's 'Prelude.Maybe' which has many
-- numeric, 'U.Unbox' and 'Storable' instances implemented.
--
-- 'Storable and 'U.Unbox' instances use a separate mask value to indicate
-- the presence or absence of a value. This allows any value of the
-- domain to be used safely in computations at the cost of a larger memory
-- footprint and possibly worse cache locality.
data Maybe a
  = Nothing
  | Just !a
  deriving (Eq, Ord, Show, Read, Typeable, Functor)

-- | A newtype of Maybe which uses a special 'nullValue' to indicate the absence
-- of data.
--
-- Unbox and Storable instances are more efficient but risk producing garbage if
-- a 'Just nullValue' is ever produced and manipulated. This is not checked
-- for performance reasons, which is the reason this type is implemented after all.
newtype Nullable a = Null { unNullable :: Maybe a}
  deriving (Eq, Ord, Show, Typeable, Functor, Applicative, Num, Fractional, NFData)

instance Newtype (Nullable a) (Maybe a) where
  pack   = Null
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

--TBD: Implement 'Floating' and 'RealFloat' instances

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

type MaskType = Word8

instance Storable a => Storable (Maybe a) where
  sizeOf _    = sizeOf (undefined :: a) + sizeOf (undefined :: MaskType)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined :: a) --XXX: Is this True?
  {-# INLINE alignment #-}
  peek p = do
    let pm = castPtr p :: Ptr MaskType
    m <- peek pm
    if m==0 then return $! Nothing else Just <$> peek (castPtr (plusPtr pm 1))
  {-# INLINE peek #-}
  poke p v =
    case v of
      Just v' -> poke pm 1 >> poke (castPtr (plusPtr pm 1)) v'
      _       -> poke pm 0
    where pm = castPtr p :: Ptr MaskType
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




newtype instance U.Vector    (Maybe a) = V_Maybe  (U.Vector MaskType, U.Vector a)
newtype instance U.MVector s (Maybe a) = MV_Maybe (UM.MVector s MaskType, UM.MVector s a)
instance U.Unbox a => U.Unbox (Maybe a)

instance U.Unbox a => G.Vector U.Vector (Maybe a) where
  basicUnsafeFreeze (MV_Maybe (x,v)) =
    V_Maybe <$> ((,) <$> G.basicUnsafeFreeze x <*> G.basicUnsafeFreeze v)
  {-# INLINE basicUnsafeFreeze #-}

  basicUnsafeThaw (V_Maybe (x,v)) =
    MV_Maybe <$> ((,) <$> G.basicUnsafeThaw x <*> G.basicUnsafeThaw v)
  {-# INLINE basicUnsafeThaw #-}

  basicLength  (V_Maybe (_,v)) = G.basicLength v
  {-# INLINE basicLength #-}

  basicUnsafeSlice m n (V_Maybe (x,v)) =
    V_Maybe (G.basicUnsafeSlice m n x, G.basicUnsafeSlice m n v)
  {-# INLINE basicUnsafeSlice #-}

  basicUnsafeIndexM (V_Maybe (x,v)) i = do
    m <- G.basicUnsafeIndexM x i
    if m/=0 then Just <$> G.basicUnsafeIndexM v i
            else return $! Nothing
  {-# INLINE basicUnsafeIndexM #-}

instance U.Unbox a => M.MVector U.MVector (Maybe a) where
  basicLength (MV_Maybe (_,v)) = M.basicLength v
  {-# INLINE basicLength #-}

  basicUnsafeSlice m n (MV_Maybe (x,v)) =
    MV_Maybe (M.basicUnsafeSlice m n x, M.basicUnsafeSlice m n v)
  {-# INLINE basicUnsafeSlice #-}

  basicOverlaps (MV_Maybe (_,v)) (MV_Maybe (_,v')) = M.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}

  basicUnsafeNew i =
    MV_Maybe <$> ((,) <$> M.basicUnsafeNew i <*> M.basicUnsafeNew i)
  {-# INLINE basicUnsafeNew #-}

  basicUnsafeRead (MV_Maybe (x,v)) i = do
    m <- M.basicUnsafeRead x i
    if m/=0 then Just <$> M.basicUnsafeRead v i
            else return $! Nothing
  {-# INLINE basicUnsafeRead #-}

  basicUnsafeWrite (MV_Maybe (x,v)) i =
    maybe (M.basicUnsafeWrite x i 0)
          (\a -> M.basicUnsafeWrite x i 1 >> M.basicUnsafeWrite v i a)

#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_Maybe (x,v)) = M.basicInitialize x >> M.basicInitialize v
  {-# INLINE basicInitialize #-}
#endif

-- | Create a vector of 'Maybes's from a vector of the underlying
-- type and a vector of 'MaskType's without copying.
--
-- Time complexity: O(1)
--
-- This can be used to interface with external sources which provide
-- a mask array
fromMaskAndVector
  :: U.Unbox a => U.Vector MaskType -> U.Vector a -> U.Vector (Maybe a)
fromMaskAndVector vm v = V_Maybe (G.unsafeSlice 0 len vm, G.unsafeSlice 0 len v)
  where len = min (G.length vm) (G.length v)
{-# INLINE fromMaskAndVector #-}

-- | Unwrap a vector of 'Maybes's to a vector of the underlying
-- type and a vector of 'MaskType's without copying them
--
--  Time complexity: O(1)
--
-- This can be used to interface with external sources
-- (eg: by converting them to 'Storable' vectors)
toMaskAndVector
  :: U.Vector (Maybe a) -> (U.Vector MaskType, U.Vector a)
toMaskAndVector (V_Maybe a) = a
{-# INLINE toMaskAndVector #-}
