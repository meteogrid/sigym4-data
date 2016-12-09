{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Sigym4.Data.Units (
  module Sigym4.Data.Units
, module Numeric.IEEE
, module DP
, module Data.Default
) where

import           Sigym4.Data.Null (HasNull)
import           Data.Default (Default(..))
import           Numeric.IEEE (IEEE(..))
import qualified Numeric.Units.Dimensional as DP

type family UnitQuantity (u :: * -> *) a = q

class ( Num (e q)
      , HasNull (e q)
      , Default (Units q)
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

instance ( Num (e (DP.Unit k u a))
         , HasNull (e (DP.Unit k u a))
         , Default (DP.Unit k u a)
         ) => HasUnits (DP.Unit k u a) e where
  type Units (DP.Unit k u a) = DP.Unit k u a
  type MachineType (DP.Unit k u a) = a
  (*~) = undefined -- (DP.*~) --TBD
  (/~) = undefined -- (DP./~) --TBD
  {-# INLINE (*~) #-}
  {-# INLINE (/~) #-}
