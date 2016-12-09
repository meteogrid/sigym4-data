{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Sigym4.Data (
-- * Typeclasses principales
  HasCalculateFingerprint (..)
, HasDimension (..)
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
, prettyAST

-- * Tipos re-exportados
, Storable
, Vector
, MonadError (..)

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

import Control.Monad.Except (MonadError(..))
import Data.Vector.Storable (Storable, Vector)
