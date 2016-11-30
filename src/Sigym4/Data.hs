{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Sigym4.Data (
-- * Tipos principales
  Variable ((:<|>))

-- ** Los distintos tipos de geometria de una variable
, VariableType
, RasterT
, AreaT
, LineT
, PointT

-- ** Restricciones que deben satisfacer todas las 'Variable's
, IsVariable

-- ** Restricciones que deben satisfacer todas las 'Variable's de
-- entrada
, IsRasterInput
, IsVectorInput

-- * Constructores de 'Variable's

-- ** Adaptadores

-- *** 'adaptDim'
, adaptDim
, CanAdaptDim

-- *** 'warp'
, warp
, CanWarp

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

-- *** 'rasterize'
, rasterize
, CanRasterize

-- *** 'sample'
, sample
, CanSample

-- *** 'aggregate'
, aggregate
, CanAggregate

-- ** 'Variable's puras
-- *** 'const'
, const
-- *** 'ofDimension'
, ofDimension

-- ** Combinadores de variables
-- *** 'map'
, map
-- *** 'zipWith'
, zipWith

-- ** Metadatos de 'Variable's
-- *** 'checkpoint'
, checkpoint
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
, getFingerprint
, prettyAST
, dimension

-- * Tipos re-exportados
, Storable
, Vector

-- * Modulos re-exportados
, module Sigym4.Data.Fingerprint
, module Sigym4.Data.Units
, module Sigym4.Dimension
, module Sigym4.Dimension.CronSchedule
, module Sigym4.Geometry
, module SpatialReference
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
