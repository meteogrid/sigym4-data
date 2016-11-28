{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Sigym4.Data (
  Variable ((:<|>))
, VariableType (..)
, MissingInput (..)
, SomeDimensionIx (..)
, LoadError (..)
, Description
, RasterT
, AreaT
, PointT
, adaptDim
, dimension
, getFingerprint
, isInput
, isDerived
, ofDimension
, warp
, grid
, rasterize
, sample
, aggregate
, checkpoint
, describe
, map
, zipWith
, getMissingInputs
, prettyAST
, module Sigym4.Data.Fingerprint
, module Sigym4.Data.Units
, module Sigym4.Dimension
, module Sigym4.Dimension.CronSchedule
, module Sigym4.Geometry
, module SpatialReference
, Storable
, Vector
) where

import Sigym4.Data.Generic
import Sigym4.Data.Units
import Sigym4.Data.Fingerprint
import Sigym4.Data.AST

import Sigym4.Geometry
import Sigym4.Dimension hiding (Dim)
import SpatialReference
import Sigym4.Dimension.CronSchedule

import Data.Vector.Storable (Storable, Vector)
