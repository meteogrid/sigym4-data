{-# LANGUAGE Trustworthy #-}
module Sigym4.Data (
  module Sigym4.Data.AST
, module Sigym4.Data.Generic
, module Sigym4.Data.Units
, module Sigym4.Dimension
, module Sigym4.Dimension.CronSchedule
, module Sigym4.Geometry
, module SpatialReference
, Storable
, Vector
) where

import Sigym4.Data.Generic
  ( Variable ((:<|>))
  , VariableType (..)
  , RasterT
  , AreaT
  , PointT
  , adaptDim
  , dimension
  , fingerprint
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

  , getMissingInputs
  , prettyAST
  )

import Sigym4.Data.Units
import Sigym4.Data.AST
  ( Fingerprint
  , WithFingerprint(..)
  , HasFingerprint
  , LoadError (..)
  , Description
  )

import Sigym4.Geometry
import Sigym4.Dimension hiding (Dim)
import SpatialReference
import Sigym4.Dimension.CronSchedule

import Data.Vector.Storable (Storable, Vector)
