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

module Sigym4.Data.Maybe (
    Maybe(..)
  , isNothing
  , isJust
  , fromMaybe
  , maybe
  , toVectorWith
  , fromVectorWith
) where

import Sigym4.Data.Null

import Control.Applicative (Applicative(..), liftA2)
import Control.DeepSeq (NFData(rnf))

import Data.Typeable (Typeable)

import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Base as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Foreign.Ptr (castPtr)
import Foreign.Storable

import Prelude ( Monad(..), Functor(..), Num(..)
               , Fractional(..), Eq(..), Ord(..), Show(..), Read(..)
               , Bool(..), (.), ($!), undefined, not
               )

data Maybe a
  = Nothing
  | Just !a
  deriving (Eq, Ord, Read, Show, Typeable, Functor)

instance NFData a => NFData (Maybe a) where
  rnf (Just a) = rnf a
  rnf Nothing  = ()
  {-# INLINE rnf #-}

instance (Storable a, HasNull a) => Storable (Maybe a) where
  sizeOf _    = sizeOf (undefined :: a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined :: a)
  {-# INLINE alignment #-}
  peek p = do
    v <- peek (castPtr p)
    return $! if isNull v then Nothing else Just v
  {-# INLINE peek #-}
  poke p = poke (castPtr p) . fromMaybe nullValue
  {-# INLINE poke #-}

instance Applicative Maybe where
  pure = Just
  {-# INLINE pure #-}
  Just f   <*>  m = fmap f m
  Nothing  <*> _m = Nothing
  {-# INLINE (<*>) #-}
  Just _m1   *>  m2 = m2
  Nothing    *> _m2 = Nothing
  {-# INLINE (*>) #-}

instance Monad Maybe where
  (Just x)   >>= k = k x
  Nothing    >>= _ = Nothing
  {-# INLINE (>>=) #-}
  (>>)             = (*>)
  {-# INLINE (>>) #-}
  return           = Just
  {-# INLINE return #-}
  fail _           = Nothing
  {-# INLINE fail #-}

instance (HasNull a, Num a) => Num (Maybe a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  negate (Just n) = let n' = negate n in if isNull n' then Nothing else Just n'
  negate Nothing  = Nothing
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger v = let v' = fromInteger v in if isNull v' then Nothing else Just v'
  {-# INLINE fromInteger #-}

instance (HasNull a, Fractional a) => Fractional (Maybe a) where
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  recip = fmap recip
  {-# INLINE recip #-}
  fromRational v = let v' = fromRational v in if isNull v' then Nothing else Just v'
  {-# INLINE fromRational #-}

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False
{-# INLINE isNothing #-}

isJust :: Maybe a -> Bool
isJust = not . isNothing
{-# INLINE isJust #-}

fromMaybe :: a -> Maybe a -> a
fromMaybe v Nothing   = v
fromMaybe _ (Just v) = v
{-# INLINE fromMaybe #-}

maybe :: b -> (a -> b) -> Maybe a -> b
maybe v _ Nothing  = v
maybe _ f (Just v) = f v
{-# INLINE maybe #-}

newtype instance U.Vector    (Maybe a) = V_Maybe  { unVM  :: U.Vector a }
newtype instance U.MVector s (Maybe a) = MV_Maybe { unVMV :: UM.MVector s a }
instance (HasNull a, U.Unbox a) => U.Unbox (Maybe a)


instance (M.MVector UM.MVector a, HasNull a) => M.MVector U.MVector (Maybe a) where
  basicLength = M.basicLength . unVMV
  {-# INLINE basicLength #-}
  basicUnsafeSlice m n = MV_Maybe . M.basicUnsafeSlice m n . unVMV
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps v = M.basicOverlaps (unVMV v) . unVMV
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew = fmap MV_Maybe . M.basicUnsafeNew
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeRead v i = do
    val <- M.basicUnsafeRead (unVMV v) i
    return $! if isNull val then Nothing else Just val
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite v i = M.basicUnsafeWrite (unVMV v) i . fromMaybe nullValue
  {-# INLINE basicUnsafeWrite #-}
#if MIN_VERSION_vector(0,11,0)
  basicInitialize = M.basicInitialize . unVMV
  {-# INLINE basicInitialize #-}
#endif

instance (G.Vector U.Vector a, HasNull a) => G.Vector U.Vector (Maybe a) where
  basicUnsafeFreeze = fmap V_Maybe . G.basicUnsafeFreeze . unVMV
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw = fmap MV_Maybe . G.basicUnsafeThaw . unVM
  {-# INLINE basicUnsafeThaw #-}
  basicLength = G.basicLength . unVM
  {-# INLINE basicLength #-}
  basicUnsafeSlice m n = V_Maybe . G.basicUnsafeSlice m n . unVM
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM v i = do
    val <- G.basicUnsafeIndexM (unVM v) i
    return $! if isNull val then Nothing else Just val
  {-# INLINE basicUnsafeIndexM #-}

fromVectorWith
  :: (G.Vector v a, G.Vector v (Maybe a), Eq a )
  => a -> v a -> v (Maybe a)
fromVectorWith nd = G.map (\v -> if v==nd then Nothing else Just v)
{-# INLINE fromVectorWith #-}

toVectorWith
  :: (G.Vector v a, G.Vector v (Maybe a))
  => a -> v (Maybe a) -> v a
toVectorWith nd = G.map (fromMaybe nd)
{-# INLINE toVectorWith #-}
