{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Sigym4.Data (
-- * Typeclasses principales
  HasCalculateFingerprint (..)
, HasDimension (..)
, HasLoad (..)
-- * Tipos principales
, Variable ((:<|>))
-- ** Distintos tipos de geometria de una variable
, RasterT
, AreaT
, LineT
, PointT

-- ** Restricciones que deben satisfacer todas las 'Variable's
, IsVariable

-- * Constructores de 'Variable's

-- ** Adaptadores

-- *** 'adaptDim'
, adaptDim
, CanAdaptDim

-- *** 'warp'
, warp
, warpWith
, WarpSettings
, pixelSize
, maxExtent
, upsampleAlgorithm
, downsampleAlgorithm

, Warpable
, ResampleAlgorithm (..)
, Up
, Down

-- ** Alternativas

-- | Selecciona la primera variable si esta disponible, sino la segunda.
--
--   Es un operador asociativo y se puede encadenar, ie:
--
--   >>> opcion1 :<|> (opcion2 :<|> opcion3)
--    = (opcion1 :<|> opcion2) :<|> opcion3
--    = opcion1 :<|> opcion2 :<|> opcion3

-- *** ':<|>'

-- ** Conversiones entre tipos de 'Variable'

-- *** 'grid'
, grid
, CanGrid
--
-- *** 'contour'
, contour
, Contourable

-- *** 'rasterize'
, rasterize
, CanRasterize

-- *** 'sample'
, sample
, CanSample

-- *** 'aggregate'
, aggregate
, CanAggregate

-- ** 'Variable's de entradda
-- *** 'input'
, IsInput
, input
-- ** 'Variable's puras
-- *** 'const'
, IsConst
, const
-- *** 'ofDimension'
, ofDimension

-- ** Combinadores de variables
-- *** 'Interpretable'
, Interpretable
-- *** 'foldDim'
, foldDim
-- *** 'map'
, map
-- *** 'mapReduce'
, mapReduce
-- *** 'zipWith'
, zipWith

-- ** Metadatos de 'Variable's
-- *** 'describe'
, describe

-- * Otros tipos
-- ** 'MissingInput'
, MissingInput (..)

-- ** 'LoadError'
, LoadError (..)

-- ** 'Description'
, Description

-- ** 'SomeDimensionIx'
, SomeDimensionIx (..)

-- * Utilidades varias
, getMissingInputs
, prettyAST

-- * Tipos re-exportados
, Default (def)
, Storable
, Vector
, MonadError (..)

-- * Re-exports
, module Data.Fingerprint
, module Sigym4.Dimension
, module Sigym4.Dimension.CronSchedule
, module Sigym4.Geometry
, module SpatialReference
, module Control.Lens
, module V1
, module V2
, module V3
, module V4
, module VN
, module Linear.Epsilon
) where

import Sigym4.Data.Generic
import Sigym4.Data.AST

import Sigym4.Geometry (
    Vertex
  , SqMatrix
  , Point (..)
  , Pixel (..)
  , Size (..)
  , Offset (..)
  , Extent (..)
  , GeoTransform (..)
  , GeoReference (..)
  , mkGeoReference
  , grScalarSize
  , scalarSize
  , grForward
  , grBackward
  , eSize
  , HasVertex (..)
  )
import Sigym4.Dimension hiding (Dim)
import Sigym4.Dimension.CronSchedule
import SpatialReference

import Control.Monad.Except (MonadError(..))
import Control.Lens hiding (Const, (*~))
import Data.Default
import Data.Fingerprint
import Data.Vector.Storable (Storable, Vector)
import Linear.V1 as V1
import Linear.V2 as V2
import Linear.V3 as V3
import Linear.V4 as V4 hiding (vector, point)
import Linear.V as VN hiding (Size)
import Linear.Epsilon
