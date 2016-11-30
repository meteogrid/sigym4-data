{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Sigym4.Data (
-- * Tipos principales
-- ** Los distintos tipos de geometria de una variable
  VariableType
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

-- *** Adaptadores

-- ** 'adaptDim'
, adaptDim
, CanAdaptDim

-- ** 'warp'
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

, Variable ((:<|>))

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

-- ** Generadores de nuevas variables a partir de otras o del indice dimensional
, ofDimension
, map
, zipWith

-- ** Metadatos de 'Variable's
, checkpoint
, describe

-- * Otros tipos
-- ** 'MissingInput' es una entrada que falta calculada por 'getMissingInputs'
, MissingInput (..)

-- ** 'LoadError' son los errores que pueden darse al cargar una entrada.
, LoadError (..)

, Description

-- * 'SomeDimensionIx' es una 'Dimension' y su 'DimensionIx' asociado
, SomeDimensionIx (..)

-- * Utilidades varias
, getMissingInputs
, prettyAST
, dimension
, getFingerprint

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
