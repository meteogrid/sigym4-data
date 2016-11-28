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
import           Sigym4.Dimension
import           Sigym4.Geometry (Size, Extent, GeoReference, V2)
import           Sigym4.Geometry.Algorithms (HasExtent)
import           SpatialReference

import           Crypto.Hash (Digest, SHA1)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)
import           Data.Default
import           Data.Typeable
import           Data.Vector.Storable ( Vector, Storable )
import           GHC.TypeLits



-- | Indexa las 'Variable's por tipo de "geometria"

data VariableType
  -- | Una variable asociada a puntos
  = PointT
  -- | Una variable asociada a areas (poligonos, multipoligonos..)
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
  (exp :: * -> *)
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
    -> Variable m exp RasterT crs dim a


  -- | Una variable de entrada de puntos
  PointInput
    :: ( KnownCrs crs
       , Dimension dim
       , Show dim
       , Show (DimensionIx dim)
       , HasUnits a
       , IsVectorLayer (VectorLayer m crs a) m crs a
       )
    => { pLoad        :: DimensionIx dim
                      -> m (Either LoadError (VectorLayer m crs a))
       , pFingerprint :: DimensionIx dim
                      -> m (Either LoadError Fingerprint)
       , pDimension   :: dim
       , pDescription :: Description
       }
    -> Variable m exp PointT crs dim a

  -- | Una variable de entrada de areas
  AreaInput 
    :: ( KnownCrs crs
       , Dimension dim
       , Show dim
       , Show (DimensionIx dim)
       , HasUnits a
       )
    => { aLoad        :: DimensionIx dim
                      -> m (Either LoadError (VectorLayer m crs a))
       , aFingerprint :: DimensionIx dim
                      -> m (Either LoadError Fingerprint)
       , aDimension   :: dim
       , aDescription :: Description
       }
    -> Variable m exp AreaT crs dim a


  -- | Una variable que solo depende de la dimension
  DimensionDependant
    :: ( HasFingerprint (DimensionIx dim)
       , Dimension dim
       , Show dim
       )
    => WithFingerprint (DimensionIx dim -> a)
    -> dim
    -> Variable m exp t crs dim a


  -- | Escoge la primera variable que no de error de carga
  (:<|>)
    :: Dimension dim
    => Variable m exp t crs dim a
    -> Variable m exp t crs dim a
    -> Variable m exp t crs dim a

  -- | Reproyecta una entrada.
  --
  --   Tambien sirve para especificar un algoritmo de resampleo entre
  --   rasters del mismo sistema de referencia si no se quiere que se
  --   use el algoritmo de resampleo por defecto (vecino mas cercano)
  --   para adaptar distintas resoluciones.
  Warp
    :: ( KnownCrs crs
       , KnownCrs crs'
       , Warpable m exp t a
       , Typeable (Variable m exp t crs' dim a)
       )
    => WarpSettings m exp t          a
    -> Variable     m exp t crs' dim a
    -> Variable     m exp t crs  dim a


  -- | Interpola una variable de punto y produce una de raster.
  --
  --   Los puntos se reproyectan automaticamente al crs de salida
  --   antes de interpolar
  Grid
    :: ( KnownCrs crs
       , KnownCrs crs'
       , Griddable m exp t' a
       , Typeable (Variable m exp t' crs' dim a)
       )
    => GridSettings  m exp t'               a
    -> Variable      m exp t'      crs' dim a
    -> Variable      m exp RasterT crs  dim a
  
  -- | Convierte una variable "Rasterizable" en una raster
  --
  --   La entrada se reproyecta automaticamente al crs de salida
  Rasterize
    :: ( KnownCrs crs
       , KnownCrs crs'
       , Rasterizable m exp t' a
       , Typeable (Variable m exp t' crs' dim a)
       )
    => RasterizeSettings  m exp t'               a
    -> Variable           m exp t'      crs' dim a
    -> Variable           m exp RasterT crs  dim a


  -- | Sample
  --
  --   Produce variables de punto a partir de otros
  --   tipos de variable
  --
  --   Los puntos de entrada se reproyectan automaticamente al crs
  --   de entrada. Para Tener mas control usar Warp
  Sample
    :: ( KnownCrs crs
       , KnownCrs crs'
       , Sampleable m exp t' a
       , Typeable (Variable m exp t' crs' dim a)
       )
    => SampleSettings       m exp t'                a
    -> Variable             m exp PointT   crs  dim any
    -> Variable             m exp t'       crs' dim a
    -> Variable             m exp PointT   crs  dim a
  

  --
  -- | Aggregate
  --
  --   Agrega valores bajo areas
  --
  --   Los poligonos de entrada se reproyectan automaticamente al crs
  --   de entrada
  Aggregate
    :: ( KnownCrs crs
       , KnownCrs crs'
       , Aggregable m exp t' a
       , Typeable (Variable m exp t' crs' dim a)
       )
    => AggregateSettings m exp t'               a
    -> Variable          m exp AreaT   crs  dim any
    -> Variable          m exp t'      crs' dim a
    -> Variable          m exp AreaT   crs  dim a
    
  

  -- | Adapta dimensiones, eg de prediccion a observacion.
  --
  --   Tambien se puede usar para tener mas control sobre
  --   como se seleccionan entradas de la misma dimension pero
  --   distintos valores. Por defecto se selecciona usando dfloor
  AdaptDim ::
    ( Dimension dim
    , Dimension dim'
    , Show dim
    , Typeable (Variable m exp t crs dim' a)
    --, DimensionIx dim' ~  DimensionIx dim
    )
    => dim
    -> WithFingerprint (dim' -> DimensionIx dim -> NonEmpty (DimensionIx dim'))
    -> Variable m exp t crs dim' a
    -> Variable m exp t crs dim  a


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
    -> Variable m exp t crs dim a
    -> Variable m exp t crs dim a

  -- | Asigna una descripcion a una variable
  --
  Describe ::
       Description
    -> Variable m exp t crs dim a
    -> Variable m exp t crs dim a


  -- | Aplica una funcion unaria
  Map
    :: ( HasUnits b
       , Typeable (Variable m exp t crs dim b)
       )
    => WithFingerprint (exp b -> exp a)
    -> Variable m exp t crs dim b
    -> Variable m exp t crs dim a

deriving instance Typeable (Variable m exp t crs dim a)

class HasFingerprint o where
  fingerprint :: o -> Fingerprint

data WithFingerprint a = WithFingerprint Fingerprint a
  deriving (Eq, Ord, Show)

instance HasFingerprint (WithFingerprint a) where
  fingerprint (WithFingerprint f _) = f

instance HasFingerprint Fingerprint where
  fingerprint = id

type Fingerprint = Digest SHA1

type RasterT = 'RasterT
type PointT = 'PointT
type AreaT = 'AreaT

type family RasterBand    (m :: * -> *) crs a = r | r -> m crs a 
type family VectorLayer   (m :: * -> *) crs a = r | r -> m crs a
type family Exp   (m :: * -> *)   = (r :: * -> *) | r -> m

type Message = String

data LoadError
  -- La entrada aun no esta disponible
  = NotAvailable
  -- La entrada esta corrupta y no se debe volver a intentar
  -- abrir hasta que algun operario lo arregle
  | Corrupt         !Message
  -- La entrada no se puede cargar por algun problema transitorio.
  -- Se debe reintentar abrir pasado algun tiempo si procede
  | TransitoryError !Message
  -- Hay algun problema interno y no se debe volver a intentar
  -- abrir hasta que algun operario lo arregle
  | InternalError   !Message
  deriving (Eq, Show)



class ( HasFingerprint (WarpSettings m exp t a)
      , Default (WarpSettings m exp t a)
      , Show (WarpSettings m exp t a)
      ) => Warpable m exp t a where
  type WarpSettings m exp t a :: *
  doWarp :: (KnownCrs crs', KnownCrs crs)
         => WarpSettings m exp t          a
         -> Variable     m exp t crs' dim a
         -> Variable     m exp t crs  dim a

instance {-# OVERLAPPABLE #-}
  ( TypeError ('Text "No hay implementacion para resamplear " ':$$:
               'Text "este tipo de Variables")
  , Default (WarpSettings m exp t a)
  , HasFingerprint (WarpSettings m exp t a)
  , Show (WarpSettings m exp t a)
  ) => Warpable m exp t a where
  type WarpSettings m exp t a = ()
  doWarp = error "unreachable"



class ( HasFingerprint (RasterizeSettings m exp t a)
      , Default (RasterizeSettings m exp t a)
      , Show (RasterizeSettings m exp t a)
      ) => Rasterizable m exp t a where
  type RasterizeSettings m exp t a :: *
  doRasterize :: (KnownCrs crs', KnownCrs crs)
              => RasterizeSettings m exp t                a
              -> Variable          m exp t       crs' dim a
              -> Variable          m exp RasterT crs  dim a

instance {-# OVERLAPPABLE #-}
  ( TypeError ('Text "No hay implementacion para rasterizar " ':$$:
               'Text "este tipo de Variables")
  , Default (RasterizeSettings m exp t a)
  , HasFingerprint (RasterizeSettings m exp t a)
  , Show (RasterizeSettings m exp t a)
  ) => Rasterizable m exp t a where
  type RasterizeSettings m exp t a = ()
  doRasterize = error "unreachable"



class ( HasFingerprint (GridSettings m exp t a)
      , Default (GridSettings m exp t a)
      , Show (GridSettings m exp t a)
      ) => Griddable m exp t a where
  type GridSettings m exp t a :: *
  doGrid :: (KnownCrs crs', KnownCrs crs)
         => GridSettings m exp t                a
         -> Variable     m exp t       crs' dim a
         -> Variable     m exp RasterT crs  dim a

instance {-# OVERLAPPABLE #-}
  ( TypeError ('Text "No hay implementacion para interpolar " ':$$:
               'Text "este tipo de Variables")
  , Default (GridSettings m exp t a)
  , HasFingerprint (GridSettings m exp t a)
  , Show (GridSettings m exp t a)
  ) => Griddable m exp t a where
  type GridSettings m exp t a = ()
  doGrid = error "unreachable"




class ( HasFingerprint (SampleSettings m exp t a)
      , Default (SampleSettings m exp t a)
      , Show (SampleSettings m exp t a)
      ) => Sampleable m exp t a where
  type SampleSettings m exp t a :: *
  doSample :: (KnownCrs crs', KnownCrs crs)
           => SampleSettings m exp t               a
           -> Variable       m exp PointT crs  dim any
           -> Variable       m exp t      crs' dim a
           -> Variable       m exp PointT crs  dim a

instance {-# OVERLAPPABLE #-}
  ( TypeError ('Text "No hay implementacion para interpolar " ':$$:
               'Text "este tipo de Variables")
  , Default (SampleSettings m exp t a)
  , HasFingerprint (SampleSettings m exp t a)
  , Show (SampleSettings m exp t a)
  ) => Sampleable m exp t a where
  type SampleSettings m exp t a = ()
  doSample = error "unreachable"




class ( HasFingerprint (AggregateSettings m exp t a)
      , Default (AggregateSettings m exp t a)
      , Show (AggregateSettings m exp t a)
      ) => Aggregable m exp t a where
  type AggregateSettings m exp t a :: *
  doAggregate :: (KnownCrs crs', KnownCrs crs)
              => AggregateSettings m exp t              a
              -> Variable          m exp AreaT crs  dim any
              -> Variable          m exp t     crs' dim a
              -> Variable          m exp AreaT crs  dim a

instance {-# OVERLAPPABLE #-}
  ( TypeError ('Text "No hay implementacion para agregar  " ':$$:
               'Text "este tipo de Variables")
  , Default (AggregateSettings m exp t a)
  , HasFingerprint (AggregateSettings m exp t a)
  , Show (AggregateSettings m exp t a)
  ) => Aggregable m exp t a where
  type AggregateSettings m exp t a = ()
  doAggregate = error "unreachable"

type IsRasterInput m crs dim a =
  ( KnownCrs crs
  , Dimension dim
  , Show dim
  , Show (DimensionIx dim)
  , HasUnits a
  , IsRasterBand (RasterBand m crs a) m crs a
  )


type IsRasterBand b m crs a =
  ( HasBlockSize    b m
  , HasNodataValue  b m     a
  , HasDescription  b m
  , HasCrs          b m
  , HasRasterSize   b m
  , HasGeoReference b m crs
  , HasReadBlock    b m     a
  , Storable                a
  )

type IsVectorLayer l m crs a =
  ( HasCrs          l m
  , HasDescription  l m
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

class HasDescription b m | b -> m  where
  description :: b -> m Description

class HasSourceFingerprint m where
  sourceFingerprint :: m Fingerprint


type BlockIndex = Size V2

class Storable a => HasReadBlock b m a | b -> m, b -> a where
  readBlock :: b -> BlockIndex -> m (Vector a)
