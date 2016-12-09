{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
{-# LANGUAGE QuasiQuotes #-}
module Sigym4.Data.AST where

import           Sigym4.Data.Fingerprint
import           Sigym4.Dimension
import           Sigym4.Geometry (Size, GeoReference, V2)
import           SpatialReference

import           Control.DeepSeq (NFData(rnf))
import           Control.Exception (SomeException)
import           Control.Monad.Except (MonadError(catchError, throwError))
import           Data.Default
import           Data.Function ( on )
import           Data.Monoid ( (<>) )
import           Data.String ( fromString )
import           Data.Typeable
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Storable as St
import           GHC.TypeLits
import           GHC.Exts (Constraint)
import           Text.PrettyPrint hiding ((<>))
import           Text.Printf (printf)


-- | Una rejilla regular de puntos donde cada uno representa el
-- valor del *area* cubierta por el "pixel" reproyectado al
-- terreno ("cell value" en la terminologia que usa VTK)
data RasterT

-- | Una variable asociada a puntos (0D)
data PointT

-- | Una variable asociada a lineas (1D)
data LineT

-- | Una variable asociada a areas (2D)
data AreaT



-- | Una 'Variable' (Store en terminologia Sigym3).
--
--  Describe el algebra de composicion de variables.
--
--  Debe garantizar que si una variable se puede describir
--  (ie: "typechecks") y cargar entonces el interprete
--  producira un resultado (modulo _|_)
--
--  Que el interprete finalice ("something, something halting problem")
--  o que el resultado sea el esperado dependera unicamente de si se ha
--  descrito realmente lo que se queria. Para ello seguimos necesitando
--  humanos.
data Variable
  -- 'm' es el interprete
  (m :: * -> *)
  -- 't' es la geometria de la variable
  t
  -- 'crs' es el sistema de referencia de coordenadas.
  crs
  -- 'dim' es la dimension que indexa los productos de las variables.
  -- '()' para una variable estatica, 'Observation', 'Prediction', etc..
  -- La unica restriccion es que el tipo sea instancia de 'Dimension'
  dim
  -- 'a' es el tipo principal de la variable que dependera del dominio.
  -- En Sigym4 intentaremos siempre usar instancias de 'HasUnits' para
  -- disfrutar de conversion automatica de unidades compatibles y mas seguridad
  -- el la definicion de variables vigilada por el compilador.
  a
  where

  -- | Una variable externa que no sabemos generar pero si cargar
  -- desde alguna parte
  Input
    :: IsInput m t crs dim a
    => Loader m t crs dim a
    -> Variable m t crs dim a
  
  -- | Una variable constante en el espacio independiente del indice
  -- dimensional
  Const
    :: IsConst dim a
    => dim -> a -> Variable m t crs dim a

  -- | Una variable constante en el espacio dependiente del indice
  -- dimensional
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

  -- | Crea isolineas a partir de un raster
  --
  Contour
    :: Contourable m a
    => ContourSettings    m                  a
    -> Variable           m RasterT crs  dim a
    -> Variable           m LineT   crs  dim a

  -- | Reproyecta una variable
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
    -> (DimensionIx dim -> [DimensionIx dim'])
    -> Variable m t crs dim' a
    -> Variable m t crs dim  a

  -- | Adapta una variable de otro interprete
  Hoist
    :: (IsVariable m' t crs dim a, Hoistable m m')
    => Variable  m' t crs dim a
    -> Variable  m  t crs dim a

  -- | Asigna una descripcion a una variable
  --
  Describe ::
       Description
    -> Variable m t crs dim a
    -> Variable m t crs dim a


  -- | Un left-fold sobre todos los indices dimensionales dada una funcion 'f',
  -- un valor inicial (ie: (ix0,v0)) y una variable b.
  --
  -- Sirve para implementar variables recursivas (eg: canadiense, dias sin
  -- precipitacion, etc)
  FoldDim
    :: ( Interpretable m t a
       , Interpretable m t b
       , IsVariable m t crs dim b
       )
    => WithFingerprint (Exp m a -> Exp m b -> Exp m a)
    -> (DimensionIx dim, Variable m t crs dim a)
    -> Variable m t crs dim b
    -> Variable m t crs dim a

  -- | Una reduccion sobre los indices dimensionales.
  --
  --   * (a -> b -> a) es la funcion que acumula en a los valores b de la
  --     variable que reducimos (ie: "map")
  --   * (a -> a -> a) es la funcion que agrega los resultados parciales
  --   (ie: "reduce")
  --   Es muy importante que "reduce" sea una funcion asociativa y conmutativa
  --   ya que el orden de evaluacion de "map" no esta definido.
  --
  -- Sirve para implementar medias, medianas, modas, desviaciones tipicas...
  -- Ver http://static.googleusercontent.com/media/research.google.com/es/us/archive/mapreduce-osdi04.pdf
  -- para mas informacion
  MapReduce
    :: ( Interpretable m t a
       , Interpretable m t b
       , IsVariable m t crs dim a
       , IsVariable m t crs dim b
       )
    => WithFingerprint (Exp m a -> Exp m b -> Exp m a)
    -> WithFingerprint (Exp m a -> Exp m a -> Exp m a)
    -> a
    -> (DimensionIx dim -> [DimensionIx dim])
    -> Variable m t crs dim b
    -> Variable m t crs dim a


  -- | Aplica una funcion unaria
  Map
    :: ( Interpretable m t a
       , Interpretable m t b
       , IsVariable m t crs dim b
       )
    => WithFingerprint (Exp m b -> Exp m a)
    -> Variable m t crs dim b
    -> Variable m t crs dim a

  ZipWith
    :: ( Interpretable m t a
       , Interpretable m t b
       , Interpretable m t c
       , IsVariable m t crs dim b
       , IsVariable m t crs dim c
       )
    => WithFingerprint (Exp m b -> Exp m c -> Exp m a)
    -> Variable m t crs dim b
    -> Variable m t crs dim c
    -> Variable m t crs dim a

infixl 3 :<|>

-- | Restriccion que deben satisfacer todas las 'Variable's
type IsVariable m t crs dim a  =
  ( Typeable (Variable m t crs dim a)
  , Typeable m, Typeable t, Typeable crs, Typeable dim, Typeable a
  , HasDescription (Variable m t crs dim a)
  , Show dim
  , Dimension dim
  , Dependent dim ~  ()
  , Show (DimensionIx dim)
  , Eq (DimensionIx dim)
  , Ord (DimensionIx dim)
  , NFData (DimensionIx dim)
  , HasFingerprint (DimensionIx dim)
  , HasFingerprint a
  , HasExp m a
  , NFData a
  , MonadError LoadError m
  )

instance
  ( IsVariable m t crs dim a
  , Interpretable m t a
  , Lift m a
  , Num a
  ) => Num (Variable m t crs dim a) where
  (+) = ZipWith ([fp||](lift2 (+)))
  (-) = ZipWith ([fp||](lift2 (-)))
  (*) = ZipWith ([fp||](lift2 (*)))
  negate = Map ([fp||](lift1 negate))
  abs = Map ([fp||](lift1 abs))
  signum = Map ([fp||](lift1 signum))
  fromInteger = error "Variables cannot be created from numeric literals (or with fromInteger)"

instance
  ( IsVariable m t crs dim a
  , Interpretable m t a
  , Lift m a
  , Fractional a
  ) => Fractional (Variable m t crs dim a) where
  (/) = ZipWith ([fp||](lift2 (/)))
  recip = Map ([fp||](lift1 recip))
  fromRational = error "Variables cannot be created from numeric literals (or with fromRational)"

type IsInput m t crs dim a = 
  ( HasLoad                   m t crs dim a
  , HasCalculateFingerprint   m dim (Loader m t crs dim a)
  , HasDimension              (Loader m t crs dim a) dim
  , HasDescription            (Loader m t crs dim a)
  , NFData                    (Loader m t crs dim a)
  , Show                      dim
  )

-- | Restriccion que impone el interpete para que una variable sea interpretable
type family Interpretable (m :: * -> *) (t :: *) (a :: *) :: Constraint

-- | Restriccion que deben satisfacer las 'Variable's que podemos
--   adaptar sus indices dimensionales
type CanAdaptDim m t crs dim' dim a =
  ( Dimension dim
  , Dimension dim'
  , NFData dim'
  , Show dim
  , Show (DimensionIx dim)
  , Show dim'
  , Show (DimensionIx dim')
  , HasFingerprint (DimensionIx dim')
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
instance NFData dim => NFData (Variable m t crs dim a)
  where

  rnf (Input i) = rnf i
  rnf (Const d v) = rnf d `seq` rnf v
  rnf (DimensionDependant v1 v2) = rnf v1 `seq` rnf v2
  rnf (v1 :<|> v2) = rnf v1 `seq` rnf v2
  rnf (Warp v1 v2) = rnf v1 `seq` rnf v2
  rnf (Grid v1 v2) = rnf v1 `seq` rnf v2
  rnf (Contour v1 v2) = rnf v1 `seq` rnf v2
  rnf (Rasterize v1 v2) = rnf v1 `seq` rnf v2
  rnf (Sample v1 v2 v3) = rnf v1 `seq` rnf v2 `seq` rnf v3
  rnf (Aggregate v1 v2 v3) = rnf v1 `seq` rnf v2 `seq` rnf v3
  rnf (Hoist w) = rnf w
  rnf (AdaptDim v1 v2 v3) = rnf v1 `seq` rnf v2 `seq` rnf v3
  rnf (MapReduce m r z s v) = rnf m `seq` rnf r `seq` rnf z `seq` rnf s `seq` rnf v
  rnf (FoldDim z f v) = rnf z `seq` rnf f `seq` rnf v
  rnf (Describe v1 v2) = rnf v1 `seq` rnf v2
  rnf (Map v1 v2) = rnf v1 `seq` rnf v2
  rnf (ZipWith v1 v2 v3) = rnf v1 `seq` rnf v2 `seq` rnf v3



class HasExp (m :: * -> *) a where
  type Lift m a :: Constraint
  type Lift m a = ()
  data Exp m a :: *
  lift :: Lift m a => a -> Exp m a
  lift1 :: Lift m b => (a -> b) -> Exp m a -> Exp m b
  lift2 :: Lift m c => (a -> b -> c) -> Exp m a -> Exp m b -> Exp m c

type Message = String

data LoadError

  -- | La entrada aun no esta disponible, no hay nada que hacer...
  = NotAvailable

  -- | La entrada esta corrupta. El interprete es libre cachear este
  -- hecho y no volver a intentar generar la 'Variable' hasta que
  -- algun operario/proceso haga algo (eg: reemplazar la entrada por una
  -- buena, volver a intentar descargar un fichero, etc...)
  --
  -- *NO* se debe meter a mano un fichero constante o algo similar.
  -- Lo correcto es explicitamente reflejarlo en el sistema modificando
  -- la variable para devolver la constante (o alternativa, ...)
  -- cuando el indice dimensional este en la "lista negra".
  --
  -- Por ejemplo:
  --
  -- >>> import qualified Sigym4.Data as D
  -- >>> let rota = undefined -- la variable con "agujeros" que queremos tapar
  --         -- Inserta una alternativa que probara
  --         arreglada = adaptDim (dimension rota) arreglador (D.const 0)
  --                :<|> rota
  --         arreglador _ ix
  --           -- Si el indice pertenece a la lista negra decimos que es
  --           -- valido para que se use la constante, si no decimos que
  --           -- no hay adaptacion posible para que se use la variable
  --           -- que arreglamos
  --           | ix `elem` listaNegra = [ix]
  --           | otherwise            = []
  --        listaNegra = [...]
  | Corrupt         Message

  -- | La entrada no se puede cargar por algun problema transitorio
  -- (eg: problema de conectividad con servicios remotos, sobrecarga
  -- en el sistema, etc...)
  --
  -- El interprete debe reintentar generar o cargar la variable pasado
  -- algun tiempo si alguien vuelve a solicitarla.
  | TransitoryError Message

  -- | Hay algun problema interno que el interprete esperaba.
  --
  -- El interprete es libre de cachear esta informacion y no volver
  -- a intentar cargar esta variable hasta que algun operrario/proceso
  -- lo arregle
  | InternalError   Message

  -- | Alguna adaptacion de dimension ha devuelto una lista vacia.
  -- Esto es semanticamente lo mismo que 'NotAvailable' pero devolvemos
  -- esto para dar mas informacion al usuario.
  | DimAdaptError

  -- | Alguna excepcion que no hemos sabido gestionar. El interprete es
  -- libre de hacer lo que quiera (probablemente sea su culpa)
  | LoadException   SomeException

  -- | Se ha sobrepasado la altura maxima del grafo de generacion
  --   Probablemente exita un ciclo
  | MaxDepthExceeded

  deriving Show

instance NFData LoadError where
  rnf !NotAvailable          = ()
  rnf !(Corrupt msg)         = rnf msg
  rnf !(TransitoryError msg) = rnf msg
  rnf !(InternalError msg)   = rnf msg
  rnf !DimAdaptError         = ()
  rnf !(LoadException !_)    = () --FIXME: This doesn't evaluate SomeException to nf!
  rnf !MaxDepthExceeded      = ()

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


class (Monad m, Monad m') => Hoistable m m' where hoist :: m' a -> m a

class ( HasFingerprint (ContourSettings m a)
      , Default (ContourSettings m a)
      , Show (ContourSettings m a)
      , NFData (ContourSettings m a)
      ) => Contourable m a where
  type ContourSettings m a :: *
  doContour
    :: ContourSettings m                 a
    -> Variable        m RasterT crs dim a
    -> Variable        m LineT   crs dim a
instance {-# OVERLAPPABLE #-}
  ( TypeError ('Text "Este interprete no sabe crear contornos ")
  , Default (ContourSettings m a)
  , HasFingerprint (ContourSettings m a)
  , Show (ContourSettings m a)
  , NFData (ContourSettings m a)
  ) => Contourable m a where
  type ContourSettings m a = ()
  doContour = error "unreachable"

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


type IsConst dim a =
  ( NFData a
  , HasFingerprint a
  , Show a
  , Dimension dim
  , Show dim
  )

data family Loader (m :: * -> *) t crs dim a :: *

class IsVariable m t crs dim a => HasLoad m t crs dim a where
  type Dataset   m t crs     a :: *
  load
    :: Loader m t crs dim a
    -> DimensionIx dim
    -> m (Dataset m t crs a)

class MonadError LoadError m
  => HasCalculateFingerprint m dim a | a -> m, a -> dim where
  -- | Calcula la 'Fingerprint' de un valor de tipo 'a', para un
  -- @'DimensionIx' dim@ en un interperprete 'm'
  --
  --   Se debe garantizar que si la huella de 'a' no ha cambiado su
  --  "contenido" tampoco.
  --
  --   El interprete *debe* calcular esto eficientemente para las
  --   entradas, cacheando agresivamente si puede ya que esto se
  --   hace muy a menudo.
  calculateFingerprint :: a -> DimensionIx dim -> m Fingerprint

class HasDimension o dim | o->dim where
  dimension :: o -> dim

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

class IsStorableVector (BlockVectorType b m) a => HasReadBlock b m a | b -> m, b -> a where
  type BlockVectorType b m :: * -> *
  readBlock :: b -> BlockIndex -> m (BlockVectorType b m a)

class (St.Storable (StorableElem v a), G.Vector v a) => IsStorableVector v a where
  type StorableElem v a :: *
  toStorable   :: v a -> St.Vector (StorableElem v a)
  fromStorable :: St.Vector (StorableElem v a) -> v a

instance St.Storable a => IsStorableVector St.Vector a where
  type StorableElem St.Vector a = a
  toStorable   = id
  fromStorable = id

-- | Crea un 'Doc' con el arbol de sintaxis de una variable
prettyAST
  :: IsVariable m t crs dim a
  => Variable m t crs dim a -> Doc
prettyAST = go maxDepth where
  maxDepth = 100 :: Int
  go, goN :: forall m t crs dim a. IsVariable m t crs dim a
          => Int -> Variable m t crs dim a -> Doc
  goN !n = go (n-1)
  go !n _ | n==0 = nest 2 "..."
  go !_ (Input l) = withBullet
    "Input  " <+> doubleQuotes (text (T.unpack (description l)))
                  <+> parens (text (show (dimension l)))
  go !_ (DimensionDependant _ dim) =
    "DimensionDependant" <+> (text (show dim))
  go !_ (Const d v) = "Constant" <+> text (show v) <+> parens (text (show d))
  go !n (s1 :<|> s2) =
    nextVar (goN n s1) $+$ ":<|>" $+$ nextVar (goN n s2)
  go !n (Contour s1 s2) =
    withBullet "Contour" <+> text (show s1) $$
      nextVar (goN n s2)
  go !n (Warp s1 s2) =
    withBullet "Warp" <+> text (show s1) $$
      nextVar (goN n s2)
  go !n (Grid s1 s2) =
    withBullet "Grid" <+> text (show s1) $$
      nextVar (goN n s2)
  go !n (Rasterize s1 s2) =
    withBullet "Rasterize" <+> text (show s1) $$
      nextVar (goN n s2)
  go !n (Sample s1 _ s2) =
    withBullet "Sample" <+> text (show s1) $$
      nextVar (goN n s2)
  go !n (Aggregate s1 _ s2) =
    withBullet "Aggregate" <+> text (show s1) $$
      nextVar (goN n s2)
  go !n (AdaptDim dim _ s2) =
    withBullet "AdaptDim" <+> text (show dim) $$
      nextVar (goN n s2)
  go !n (Hoist w) =
    withBullet "Hoist" $$
      nextVar (goN n w)
  go !n (MapReduce _ _ _ _ v) =
    withBullet "MapReduce" $$
      nextVar (goN n v)
  go !n (FoldDim _ (i,z) v) =
    withBullet "FoldDim" <+> text (show i) $$
      "base case: " <+> text (show i) $+$ nextVar (goN n z) $$
      nextVar (goN n v)
  go !n (Describe desc var) =
    text (T.unpack desc) <+> prettyVarType var $+$
      nextVar (goN n var)
  go !n (Map _ s2) =
    withBullet "Map" $$ nextVar (goN n s2)
  go !n (ZipWith _ a b) =
    withBullet "ZipWith" $+$
      nextVar (goN n a) $+$
      nextVar (goN n b)

  nextVar = nest 2
  withBullet = ("*" <+>)

  prettyVarType
    :: forall m t crs dim a. IsVariable m t crs dim a
    => Variable m t crs dim a -> Doc
  prettyVarType _ =
    text $ printf ":: %s %s (%s, %s)"
      (show (typeOf (undefined :: t)))
      (show (typeOf (undefined :: a)))
      (show (typeOf (undefined :: crs)))
      (show (typeOf (undefined :: dim)))


-- | Implementacion por defecto para mostrar cualquier 'Variable'
-- valida. Muestra su AST bonito.
instance IsVariable m t crs dim a => Show (Variable m t crs dim a)
  where show = show . prettyAST

instance HasDescription (Variable m t crs dim a) where
  description (Input l) = description l
  description (Const _ v) = "Constant " <> fromString (show v)
  description (DimensionDependant _ _) = "Function of dimension"
  description (v :<|> w) = description v <> " or " <> description w
  description (Contour _ v) = "Contours " <> description v
  description (Warp _ v) = "Warped " <> description v
  description (Grid _ v) = "Gridded " <> description v
  description (Rasterize _ v) = "Rasterized " <> description v
  description (Sample _ v w) = description w <> " sampled over " <> description v
  description (Aggregate _ v w) = description w <> " aggregated over " <> description v
  description (AdaptDim d _ w) = description w <> " adapted to " <> fromString (show d)
  description (Hoist w)        = description w <> "(hoisted)"
  description (FoldDim _ _ w) = "FoldDim over " <> description w
  description (MapReduce _ _ _ _ w) = "MapReduce over " <> description w
  description (Describe v _) = v
  description (Map _ v) = "Function of " <> description v
  description (ZipWith _ v w) = "Function of " <> description v <> " and " <> description w

instance HasDimension (Variable m t crs dim a) dim where
  dimension (Input l)                 = dimension l
  -- Para poder escribir la siguiente ecuacion es el unico motivo por el
  -- cual 'Const' lleva dimension asociada. Pero tiene sentido ya que 'Const'
  -- solo implica que es constante en el espacio, no en el eje dimensional que
  -- corresponda
  dimension (Const d _)              = d
  dimension (DimensionDependant _ d) = d
  -- Esto es dudoso... cual es la dimension de una alternativa?
  -- Ahora mismo no podemos decir "la de la que se seleccione" porque
  -- esto es una funcion pura que no depende de si se puede generar o
  -- no.
  -- Creo (AVG) que es "moralmente correcto" decir que la de la opcion
  -- ideal
  dimension (v :<|> _)               = dimension v
  dimension (Contour _ v)            = dimension v
  dimension (Warp _ v)               = dimension v
  dimension (Grid _ v)               = dimension v
  dimension (Rasterize _ v)          = dimension v
  dimension (Sample _ _ v)           = dimension v
  dimension (Aggregate _ _ v)        = dimension v
  dimension (Hoist w)                = dimension w
  dimension (AdaptDim d _ _)         = d
  dimension (FoldDim _ _ v)          = dimension v
  dimension (MapReduce _ _ _ _ v)    = dimension v
  dimension (Describe _ v)           = dimension v
  dimension (Map _ v)                = dimension v
  -- En las aplicaciones de mas de una variable cogemos la dimension
  -- de la primera variable siempre. Si se necesita mas control se
  -- puede envolver con adaptDim
  dimension (ZipWith _ v _)          = dimension v



instance MonadError LoadError m
  => HasCalculateFingerprint m dim (Variable m t crs dim a) where
  -- La huella de las variables derivadas siempre se puede
  -- calcular sin calcular las variables en si
  -- por lo que es muy barato saber si hay que regenerar algo.
  -- Ya sea por cambio en las entradas (eg: llegan nuevas
  -- observaciones de estacion, llega fichero corregido tras envio
  -- de fichero corrupto, etc) o por cambios en el codigo
  -- (asumiendo que el 'Fingerprint' de las funciones envueltas con 
  -- 'WithFingerprint' se genere correctamente).
  calculateFingerprint (Input l)  = calculateFingerprint l

  -- La de una constante es la de su valor. La dimension no nos importa.
  -- (Asumimos que se calcula muy rapido)
  calculateFingerprint (Const _ v) =  const . return $ fingerprint v

  -- La de una funcion del indice dimensional es funcion de su resultado
  -- (Asumimos que se calcula muy rapido)
  calculateFingerprint (DimensionDependant f _)   = return . fingerprint . f

  calculateFingerprint (Hoist a) = hoist . calculateFingerprint a

  -- La huella de una alternativa es la de la primera opcion si
  -- se puede cargar o si no la de la segunda.
  --
  -- OJO: Asume:
  --   * Que el interprete realmente usara la primera entrada
  --     si da resultado en vez de la segunda.
  --
  --   * Que una ejecucion del interprete produce entrada para
  --   la variable o no y esto no cambia dentro de la misma ejecucion
  --
  --  Es decir, que "se porta bien".
  --
  calculateFingerprint (v :<|> w) = \ix ->
    calculateFingerprint v ix `catchError` (\_ -> calculateFingerprint w ix)

  -- La huella de las operaciones intrinsecas es la huella de las
  -- variables de entrada combinada con la de la de su configuracion.
  calculateFingerprint (Contour s v)            = combineVarFPWith v s
  calculateFingerprint (Warp s v)               = combineVarFPWith v s
  calculateFingerprint (Grid s v)               = combineVarFPWith v s
  calculateFingerprint (Rasterize s v)          = combineVarFPWith v s
  calculateFingerprint (Sample s v w)           = combineVarsFPWith v w s
  calculateFingerprint (Aggregate s v w)        = combineVarsFPWith v w s
  --
  -- La huella de una adaptacion de dimension es la huella del
  -- primer indice adaptado que devuelva huella
  --
  -- OJO: Asume que el interprete realmente ejecuta la primera opcion
  --      valida, es decir, que "se porta bien".
  --
  calculateFingerprint (AdaptDim _ fun v) = \ix ->
    let loop (x:xs) = calculateFingerprint v x `catchError` const (loop xs)
        loop []     = throwError DimAdaptError

    in loop (fun ix)

  calculateFingerprint (FoldDim f (ix0,z) v) = \ix ->
    if ix==ix0 then calculateFingerprint z ix else combineVarFPWith v f ix

  -- La huella de MapReduce es la la variable de entrada, las funciones
  -- map/reduce el valor inicial del acumulador y la de todos los indices
  -- dimensionales generados por el selector
  calculateFingerprint (MapReduce m r z s v) = \ix -> do
    x <- calculateFingerprint v ix
    return (mconcat (x:fingerprint m:fingerprint r:fingerprint z:map fingerprint (s ix)))

  calculateFingerprint (Describe   _ v) = calculateFingerprint v

  -- La huella de la aplicaciones es la huella de la funcion combinada
  -- con la de sus entradas.
  calculateFingerprint (Map f v)  = combineVarFPWith v f
  calculateFingerprint (ZipWith f v w) = combineVarsFPWith v w f

combineVarFPWith
  :: (HasFingerprint o, MonadError LoadError m)
  => Variable m t crs dim a
  -> o
  -> DimensionIx dim
  -> m Fingerprint
combineVarFPWith v o ix = do
  fv <- calculateFingerprint v ix
  return (fv <> fingerprint o)

combineVarsFPWith
  :: (HasFingerprint o, MonadError LoadError m)
  => Variable m t crs dim a
  -> Variable m t' crs' dim a'
  -> o
  -> DimensionIx dim
  -> m Fingerprint
combineVarsFPWith v w o ix = do
  fv <- calculateFingerprint v ix
  fw <- combineVarFPWith w o ix
  return (mconcat [fingerprint o, fv, fw])
