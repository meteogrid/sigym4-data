{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Sigym4.Data.Units (
  module Sigym4.Data.Units
, module Numeric.IEEE
, module DP
) where

import           Data.Functor.Identity
import           Numeric.IEEE (IEEE(..))
import qualified Numeric.Units.Dimensional as DP

type family UnitQuantity (u :: * -> *) a = q

class ( Num (e (MachineType q))
      , IEEE (e (MachineType q))
      ) => HasUnits q e where
  type Units q       :: *
  type MachineType q :: *

  infixl 7 *~
  (*~) :: e (MachineType q)
       -> Units q
       -> e q


  infixl 7 /~
  (/~) :: e q
       -> Units q
       -> e (MachineType q)

type instance UnitQuantity (DP.Unit k d) a = DP.Quantity d a

instance (IEEE (e a), Num (e a))
  => HasUnits (DP.Unit k u a) e where
  type Units (DP.Unit k u a) = DP.Unit k u a
  type MachineType (DP.Unit k u a) = a
  (*~) = undefined -- (DP.*~)
  (/~) = undefined -- (DP./~)
  {-# INLINE (*~) #-}
  {-# INLINE (/~) #-}

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
