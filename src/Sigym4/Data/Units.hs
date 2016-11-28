{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Sigym4.Data.Units (
  module Sigym4.Data.Units
, module Numeric.IEEE
, module DP
) where

import           Data.Array.Accelerate (Elt, Exp, Lift)

import           Numeric.IEEE (IEEE)
import qualified Numeric.Units.Dimensional as DP

type family UnitQuantity (u :: * -> *) a = q

class ( Elt (MachineType q)
      , Lift Exp (MachineType q)
      ) => HasUnits q where
  type Units q       :: *
  type MachineType q :: *

  infixl 7 *~
  (*~) :: Exp (MachineType q)
       -> Units q
       -> Exp q


  infixl 7 /~
  (/~) :: Exp q
       -> Units q
       -> Exp (MachineType q)

type instance UnitQuantity (DP.Unit k d) a = DP.Quantity d a

instance (Elt a, Lift Exp a, IEEE a, Num a)
  => HasUnits (DP.Unit k u a) where
  type Units (DP.Unit k u a) = DP.Unit k u a
  type MachineType (DP.Unit k u a) = a
  (*~) = undefined -- (DP.*~)
  (/~) = undefined -- (DP./~)
  {-# INLINE (*~) #-}
  {-# INLINE (/~) #-}
