{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Sigym4.Data.AST where

import           Sigym4.Data.Units
import           Sigym4.Data.Fingerprint
import           Sigym4.Dimension
import           Sigym4.Geometry (Size, Extent, GeoReference, V2)
import           Sigym4.Geometry.Algorithms (HasExtent)
import           SpatialReference

import           Control.DeepSeq (NFData(rnf))
import           Control.Exception (SomeException)
import           Data.Text (Text)
import           Data.Default
import           Data.Function ( on )
import           Data.Typeable
import           Data.Vector.Storable ( Vector, Storable )
import           GHC.TypeLits



-- | Indexa las 'Variable's por tipo de "geometria"

data VariableType
  -- | Una variable asociada a puntos (0D)
  = PointT
  -- | Una variable asociada a lineas (1D)
  | LineT
  -- | Una variable asociada a areas (2D)
  | AreaT
  -- | Una variable de rejilla donde cada pixel representa el
  --   valor del *area* (cell value en terminologia VTK)
  | RasterT

-- | Una 'Variable' (Store en terminologia Sigym3).
--
--  Representa el AST del algebra de composicion de variables.
--
data Variable
  (m :: * -> *)
  (t :: VariableType)
  crs
  dim
  a
  where

  --
  -- Operaciones de carga de datos
  --
  -- Introducen variables en AST a partir de datos de entrada que no
  -- sabemos generar (o se generan por procesos externos)
  --
  
  -- | Una variable de entrada raster
  RasterInput
    :: IsRasterInput m crs dim a
    => { rLoad        :: DimensionIx dim
                      -> m (Either LoadError (RasterBand m crs a))
       , rFingerprint :: DimensionIx dim
                      -> m (Either LoadError Fingerprint)
       , rDimension   :: dim
       , rDescription :: Description
       }
    -> Variable m RasterT crs dim a


  -- | Una variable de entrada de puntos
  PointInput
    :: IsVectorInput m crs dim a
    => { pLoad        :: DimensionIx dim
                      -> m (Either LoadError (VectorLayer m crs a))
       , pFingerprint :: DimensionIx dim
                      -> m (Either LoadError Fingerprint)
       , pDimension   :: dim
       , pDescription :: Description
       }
    -> Variable m PointT crs dim a

  -- | Una variable de entrada de lineas
  LineInput 
    :: IsVectorInput m crs dim a
    => { lLoad        :: DimensionIx dim
                      -> m (Either LoadError (VectorLayer m crs a))
       , lFingerprint :: DimensionIx dim
                      -> m (Either LoadError Fingerprint)
       , lDimension   :: dim
       , lDescription :: Description
       }
    -> Variable m LineT crs dim a

  -- | Una variable de entrada de areas
  AreaInput 
    :: IsVectorInput m crs dim a
    => { aLoad        :: DimensionIx dim
                      -> m (Either LoadError (VectorLayer m crs a))
       , aFingerprint :: DimensionIx dim
                      -> m (Either LoadError Fingerprint)
       , aDimension   :: dim
       , aDescription :: Description
       }
    -> Variable m AreaT crs dim a


  -- | Una variable que solo depende de la dimension
  DimensionDependant
    :: ( HasFingerprint a
       , Dimension dim
       , Show dim
       )
    => (DimensionIx dim -> a)
    -> dim
    -> Variable m t crs dim a


  -- | Escoge la primera variable que no de error de carga
  (:<|>)
    :: Dimension dim
    => Variable m t crs dim a
    -> Variable m t crs dim a
    -> Variable m t crs dim a

  -- | Reproyecta una entrada.
  --
  --   Tambien sirve para especificar un algoritmo de resampleo entre
  --   rasters del mismo sistema de referencia si no se quiere que se
  --   use el algoritmo de resampleo por defecto (vecino mas cercano)
  --   para adaptar distintas resoluciones.
  Warp
    :: CanWarp m t crs crs' dim a
    => WarpSettings m t          a
    -> Variable     m t crs' dim a
    -> Variable     m t crs  dim a


  -- | Interpola una variable de punto y produce una de raster.
  --
  --   Los puntos se reproyectan automaticamente al crs de salida
  --   antes de interpolar
  Grid
    :: CanGrid m t' crs crs' dim a
    => GridSettings  m t'               a
    -> Variable      m t'      crs' dim a
    -> Variable      m RasterT crs  dim a
  
  -- | Convierte una variable "Rasterizable" en una raster
  --
  --   La entrada se reproyecta automaticamente al crs de salida
  Rasterize
    :: CanRasterize m t' crs crs' dim a
    => RasterizeSettings  m t'               a
    -> Variable           m t'      crs' dim a
    -> Variable           m RasterT crs  dim a


  -- | Sample
  --
  --   Produce variables de punto a partir de otros
  --   tipos de variable
  --
  --   Los puntos de entrada se reproyectan automaticamente al crs
  --   de entrada. Para Tener mas control usar Warp
  Sample
    :: CanSample m t' crs crs' dim a b
    => SampleSettings       m t'                a
    -> Variable             m PointT   crs  dim b
    -> Variable             m t'       crs' dim a
    -> Variable             m PointT   crs  dim a
  

  --
  -- | Aggregate
  --
  --   Agrega valores bajo areas
  --
  --   Los poligonos de entrada se reproyectan automaticamente al crs
  --   de entrada
  Aggregate
    :: CanAggregate m t' crs crs' dim a b
    => AggregateSettings m t'               a
    -> Variable          m AreaT   crs  dim b
    -> Variable          m t'      crs' dim a
    -> Variable          m AreaT   crs  dim a
    
  

  -- | Adapta dimensiones, eg de prediccion a observacion.
  --
  --   Tambien se puede usar para tener mas control sobre
  --   como se seleccionan entradas de la misma dimension pero
  --   distintos valores. Por defecto se selecciona usando dfloor
  AdaptDim
    :: CanAdaptDim m t crs dim' dim a
    => dim
    -> (dim' -> DimensionIx dim -> [DimensionIx dim'])
    -> Variable m t crs dim' a
    -> Variable m t crs dim  a


  -- | Indica al interprete que se esfuerze en cachear una variable
  --   cuando su indice dimensional pase el predicado.
  --
  --   Esto es util para variables recursivas (eg: canadiense, dias sin
  --   precipitacion, etc..)
  --
  --   El interprete es libre de no hacerlo o de insertar nodos de estos
  --   automaticamente si lo cree necesario.
  --
  CheckPoint ::
    ( Dimension dim
    )
    => (DimensionIx dim -> Bool)
    -> Variable m t crs dim a
    -> Variable m t crs dim a

  -- | Asigna una descripcion a una variable
  --
  Describe ::
       Description
    -> Variable m t crs dim a
    -> Variable m t crs dim a


  -- | Aplica una funcion unaria
  Map
    :: ( HasUnits b (Exp m)
       , IsVariable m t crs dim b
       )
    => WithFingerprint (Exp m b -> Exp m a)
    -> Variable m t crs dim b
    -> Variable m t crs dim a

  ZipWith
    :: ( HasUnits b (Exp m)
       , HasUnits c (Exp m)
       , IsVariable m t crs dim b
       , IsVariable m t crs dim c
       )
    => WithFingerprint (Exp m b -> Exp m c -> Exp m a)
    -> Variable m t crs dim b
    -> Variable m t crs dim c
    -> Variable m t crs dim a


-- | Restriccion que deben satisfacer todas las 'Variable's
type IsVariable m t crs dim a  =
  ( Typeable (Variable m t crs dim a)
  , Typeable m, Typeable t, Typeable crs, Typeable dim, Typeable a
  , HasDescription (Variable m t crs dim a)
  )

-- | Restriccion que deben satisfacer las 'Variable's que podemos
--   adaptar sus indices dimensionales
type CanAdaptDim m t crs dim' dim a =
  ( Dimension dim
  , Dimension dim'
  , NFData dim'
  , Show dim
  , Show (DimensionIx dim)
  , IsVariable m t crs dim' a
  )

-- | Restriccion que deben satisfacer las 'Variable's que podemos
--   reproyectar (o resamplear) a otro sistema de coordenadas (o resolucion)
type CanWarp m t crs crs' dim a =
  ( KnownCrs crs
  , KnownCrs crs'
  , Warpable m t a
  , IsVariable m t crs' dim a
  )

-- | Restriccion que deben satisfacer las 'Variable's que podemos
--   interpolar a una rejilla
type CanGrid m t crs crs' dim a =
  ( KnownCrs crs
  , KnownCrs crs'
  , Griddable  m t a
  , IsVariable m t crs' dim a
  )

-- | Restriccion que deben satisfacer las 'Variable's que podemos
--   rasterizar
type CanRasterize m t crs crs' dim a =
  ( KnownCrs crs
  , KnownCrs crs'
  , Rasterizable m t  a
  , IsVariable m t  crs' dim a
  )

-- | Restriccion que deben satisfacer las 'Variable's que podemos
--   samplear en puntos (0D)
type CanSample m t crs crs' dim a b =
  ( KnownCrs crs
  , KnownCrs crs'
  , Sampleable m t               a
  , IsVariable m PointT crs  dim b
  , IsVariable m t      crs' dim a
  )

-- | Restriccion que deben satisfacer las 'Variable's que podemos
--   agregar bajo geometrias (1D o 2D)
type CanAggregate m t crs crs' dim a b =
  ( KnownCrs crs
  , KnownCrs crs'
  , Aggregable m t              a
  , IsVariable m AreaT crs  dim b
  , IsVariable m t     crs' dim a
  )

deriving instance Typeable (Variable m t crs dim a)
instance ( NFData dim
         ) => NFData (Variable m t crs dim a)
  where

  rnf RasterInput {rLoad,rFingerprint,rDimension,rDescription} =
    rnf rLoad `seq` rnf rFingerprint
              `seq` rnf rDimension
              `seq` rnf rDescription
  rnf PointInput {pLoad,pFingerprint,pDimension,pDescription} =
    rnf pLoad `seq` rnf pFingerprint
              `seq` rnf pDimension
              `seq` rnf pDescription
  rnf LineInput {lLoad,lFingerprint,lDimension,lDescription} =
    rnf lLoad `seq` rnf lFingerprint
              `seq` rnf lDimension
              `seq` rnf lDescription
  rnf AreaInput {aLoad,aFingerprint,aDimension,aDescription} =
    rnf aLoad `seq` rnf aFingerprint
              `seq` rnf aDimension
              `seq` rnf aDescription
  rnf (DimensionDependant v1 v2) = rnf v1 `seq` rnf v2
  rnf (v1 :<|> v2) = rnf v1 `seq` rnf v2
  rnf (Warp v1 v2) = rnf v1 `seq` rnf v2
  rnf (Grid v1 v2) = rnf v1 `seq` rnf v2
  rnf (Rasterize v1 v2) = rnf v1 `seq` rnf v2
  rnf (Sample v1 v2 v3) = rnf v1 `seq` rnf v2 `seq` rnf v3
  rnf (Aggregate v1 v2 v3) = rnf v1 `seq` rnf v2 `seq` rnf v3
  rnf (AdaptDim v1 v2 v3) = rnf v1 `seq` rnf v2 `seq` rnf v3
  rnf (CheckPoint v1 v2) = rnf v1 `seq` rnf v2
  rnf (Describe v1 v2) = rnf v1 `seq` rnf v2
  rnf (Map v1 v2) = rnf v1 `seq` rnf v2
  rnf (ZipWith v1 v2 v3) = rnf v1 `seq` rnf v2 `seq` rnf v3


type RasterT = 'RasterT
type PointT = 'PointT
type LineT = 'LineT
type AreaT = 'AreaT

type family RasterBand    (m :: * -> *) crs a = r | r -> m crs a
type family VectorLayer   (m :: * -> *) crs a = r | r -> m crs a
type family Exp           (m :: * -> *)       = (r :: * -> *)

type Message = String

data LoadError
  -- La entrada aun no esta disponible
  = NotAvailable
  -- La entrada esta corrupta y no se debe volver a intentar
  -- abrir hasta que algun operario lo arregle
  | Corrupt         Message
  -- La entrada no se puede cargar por algun problema transitorio.
  -- Se debe reintentar abrir pasado algun tiempo si procede
  | TransitoryError Message
  -- Hay algun problema interno y no se debe volver a intentar
  -- abrir hasta que algun operario lo arregle
  | InternalError   Message
  -- Alguna adaptacion de dimension ha fallado
  | DimAdaptError
  -- Alguna excepcion...
  | LoadException   SomeException
  deriving Show

instance {-# OVERLAPPABLE #-}
  ( TypeError ('Text "Cannot use 'fingerprint' on Variables. Use " ':$$:
               'Text "'getFingerprint' instead")
  ) => HasFingerprint (Variable m t crs dim a) where
  fingerprint = error "unreachable"

data SomeDimensionIx where
  SomeDimensionIx :: ( Show (DimensionIx dim)
                     , Show dim
                     , Dimension dim
                     )
                  => dim -> DimensionIx dim -> SomeDimensionIx
deriving instance Show SomeDimensionIx
instance Eq SomeDimensionIx where (==) = (==) `on` show


class ( HasFingerprint (WarpSettings m t a)
      , Default (WarpSettings m t a)
      , Show (WarpSettings m t a)
      ) => Warpable m t a where
  type WarpSettings m t a :: *
  doWarp :: (KnownCrs crs', KnownCrs crs)
         => WarpSettings m t          a
         -> Variable     m t crs' dim a
         -> Variable     m t crs  dim a

instance {-# OVERLAPPABLE #-}
  ( TypeError ('Text "No hay implementacion para resamplear " ':$$:
               'Text "este tipo de Variables")
  , Default (WarpSettings m t a)
  , HasFingerprint (WarpSettings m t a)
  , Show (WarpSettings m t a)
  ) => Warpable m t a where
  type WarpSettings m t a = ()
  doWarp = error "unreachable"



class ( HasFingerprint (RasterizeSettings m t a)
      , Default (RasterizeSettings m t a)
      , Show (RasterizeSettings m t a)
      ) => Rasterizable m t a where
  type RasterizeSettings m t a :: *
  doRasterize :: (KnownCrs crs', KnownCrs crs)
              => RasterizeSettings m t                a
              -> Variable          m t       crs' dim a
              -> Variable          m RasterT crs  dim a

instance {-# OVERLAPPABLE #-}
  ( TypeError ('Text "No hay implementacion para rasterizar " ':$$:
               'Text "este tipo de Variables")
  , Default (RasterizeSettings m t a)
  , HasFingerprint (RasterizeSettings m t a)
  , Show (RasterizeSettings m t a)
  ) => Rasterizable m t a where
  type RasterizeSettings m t a = ()
  doRasterize = error "unreachable"



class ( HasFingerprint (GridSettings m t a)
      , Default (GridSettings m t a)
      , Show (GridSettings m t a)
      ) => Griddable m t a where
  type GridSettings m t a :: *
  doGrid :: (KnownCrs crs', KnownCrs crs)
         => GridSettings m t                a
         -> Variable     m t       crs' dim a
         -> Variable     m RasterT crs  dim a

instance {-# OVERLAPPABLE #-}
  ( TypeError ('Text "No hay implementacion para interpolar " ':$$:
               'Text "este tipo de Variables")
  , Default (GridSettings m t a)
  , HasFingerprint (GridSettings m t a)
  , Show (GridSettings m t a)
  ) => Griddable m t a where
  type GridSettings m t a = ()
  doGrid = error "unreachable"




class ( HasFingerprint (SampleSettings m t a)
      , Default (SampleSettings m t a)
      , Show (SampleSettings m t a)
      ) => Sampleable m t a where
  type SampleSettings m t a :: *
  doSample :: (KnownCrs crs', KnownCrs crs)
           => SampleSettings m t               a
           -> Variable       m PointT crs  dim any
           -> Variable       m t      crs' dim a
           -> Variable       m PointT crs  dim a

instance {-# OVERLAPPABLE #-}
  ( TypeError ('Text "No hay implementacion para interpolar " ':$$:
               'Text "este tipo de Variables")
  , Default (SampleSettings m t a)
  , HasFingerprint (SampleSettings m t a)
  , Show (SampleSettings m t a)
  ) => Sampleable m t a where
  type SampleSettings m t a = ()
  doSample = error "unreachable"




class ( HasFingerprint (AggregateSettings m t a)
      , Default (AggregateSettings m t a)
      , Show (AggregateSettings m t a)
      ) => Aggregable m t a where
  type AggregateSettings m t a :: *
  doAggregate :: (KnownCrs crs', KnownCrs crs)
              => AggregateSettings m t              a
              -> Variable          m AreaT crs  dim any
              -> Variable          m t     crs' dim a
              -> Variable          m AreaT crs  dim a

instance {-# OVERLAPPABLE #-}
  ( TypeError ('Text "No hay implementacion para agregar  " ':$$:
               'Text "este tipo de Variables")
  , Default (AggregateSettings m t a)
  , HasFingerprint (AggregateSettings m t a)
  , Show (AggregateSettings m t a)
  ) => Aggregable m t a where
  type AggregateSettings m t a = ()
  doAggregate = error "unreachable"

type IsRasterInput m crs dim a =
  ( KnownCrs crs
  , Dimension dim
  , Show dim
  , Show (DimensionIx dim)
  , HasUnits a (Exp m)
  , IsRasterBand (RasterBand m crs a) m crs a
  )

type IsVectorInput m crs dim a =
  ( KnownCrs crs
  , Dimension dim
  , Show dim
  , Show (DimensionIx dim)
  , HasUnits a (Exp m)
  , IsVectorLayer (VectorLayer m crs a) m crs a
  )

type IsRasterBand b m crs a =
  ( HasBlockSize    b m
  , HasNodataValue  b m     a
  , HasCrs          b m
  , HasRasterSize   b m
  , HasGeoReference b m crs
  , HasReadBlock    b m     a
  , Storable                a
  )

type IsVectorLayer l m crs a =
  ( HasCrs          l m
  , HasExtent       l   (Extent V2 crs)
  ) -- TBD


class HasBlockSize b m | b -> m where
  blockSize   :: b -> m (Size V2)

class HasNodataValue b m a | b -> m, b -> a where
  nodataValue :: b -> m (Maybe a) 

class HasCrs b m | b -> m where
  getCrs :: b -> m Crs

class HasRasterSize b m | b -> m where
  rasterSize :: b -> m (Size V2)

class HasGeoReference b m crs | b -> m, b -> crs where
  geoReference :: b -> m (GeoReference V2 crs)

type Description = Text

class HasDescription b where
  description :: b -> Description

class HasSourceFingerprint m where
  sourceFingerprint :: m Fingerprint


type BlockIndex = Size V2

class Storable a => HasReadBlock b m a | b -> m, b -> a where
  readBlock :: b -> BlockIndex -> m (Vector a)
