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
module Sigym4.Data.AST where

import           Sigym4.Data.Fingerprint
import           Sigym4.Dimension
import           Sigym4.Geometry (Size, Extent, GeoReference, V2)
import           Sigym4.Geometry.Algorithms (HasExtent)
import           SpatialReference

import           Control.DeepSeq (NFData(rnf))
import           Control.Exception (SomeException)
import           Control.Monad.Except (MonadError)
import           Data.Default
import           Data.Function ( on )
import           Data.Monoid ( (<>) )
import           Data.String ( fromString )
import           Data.Typeable
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector.Storable ( Vector, Storable )
import           GHC.TypeLits
import           GHC.Exts (Constraint)
import           Text.PrettyPrint hiding ((<>))
import           Text.Printf (printf)



-- | Indexa las 'Variable's por tipo de "geometria"

data VariableType
  = PointT
  | LineT
  | AreaT
  | RasterT

-- | Una variable de rejilla donde cada pixel representa el
-- valor del *area* cubierta por dicho pixel reproyectado al
-- terreno ("cell value" en la terminologia que usa VTK)
type RasterT = 'RasterT

-- | Una variable asociada a puntos (0D)
type PointT = 'PointT

-- | Una variable asociada a lineas (1D)
type LineT = 'LineT

-- | Una variable asociada a areas (2D)
type AreaT = 'AreaT


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
  (t :: VariableType)
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
                      -> m (RasterBand m crs a)
       , rFingerprint :: DimensionIx dim
                      -> m Fingerprint
       , rDimension   :: dim
       , rDescription :: Description
       }
    -> Variable m RasterT crs dim a


  -- | Una variable de entrada de puntos
  PointInput
    :: IsVectorInput m crs dim a
    => { pLoad        :: DimensionIx dim
                      -> m (VectorLayer m crs a)
       , pFingerprint :: DimensionIx dim
                      -> m Fingerprint
       , pDimension   :: dim
       , pDescription :: Description
       }
    -> Variable m PointT crs dim a

  -- | Una variable de entrada de lineas
  LineInput 
    :: IsVectorInput m crs dim a
    => { lLoad        :: DimensionIx dim
                      -> m (VectorLayer m crs a)
       , lFingerprint :: DimensionIx dim
                      -> m Fingerprint
       , lDimension   :: dim
       , lDescription :: Description
       }
    -> Variable m LineT crs dim a

  -- | Una variable de entrada de areas
  AreaInput 
    :: IsVectorInput m crs dim a
    => { aLoad        :: DimensionIx dim
                      -> m (VectorLayer m crs a)
       , aFingerprint :: DimensionIx dim
                      -> m Fingerprint
       , aDimension   :: dim
       , aDescription :: Description
       }
    -> Variable m AreaT crs dim a


  -- | Una variable constante
  Const
    :: IsConst dim a
    => dim -> a -> Variable m t crs dim a

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
    -> (DimensionIx dim -> [DimensionIx dim'])
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
  )

-- | Restriccion que impone el interpete para que una variable sea interpretable
type family Interpretable (m :: * -> *) (t :: VariableType) (a :: *)  :: Constraint

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
  rnf (Const d v) = rnf d `seq` rnf v
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


type family RasterBand    (m :: * -> *) crs a = r | r -> m crs a
type family VectorLayer   (m :: * -> *) crs a = r | r -> m crs a
type family Exp           (m :: * -> *)       = (r :: * -> *)

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
  , IsRasterBand (RasterBand m crs a) m crs a
  , MonadError LoadError m
  )

type IsConst dim a =
  ( NFData a
  , HasFingerprint a
  , Show a
  , Dimension dim
  , Show dim
  )

type IsVectorInput m crs dim a =
  ( KnownCrs crs
  , Dimension dim
  , Show dim
  , Show (DimensionIx dim)
  , IsVectorLayer (VectorLayer m crs a) m crs a
  , MonadError LoadError m
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
  go !_ RasterInput{rDescription,rDimension} = withBullet
    "RasterInput" <+> doubleQuotes (text (T.unpack rDescription))
                  <+> parens (text (show rDimension))
  go !_ PointInput{pDescription,pDimension} = withBullet
    "PointInput" <+> doubleQuotes (text (T.unpack pDescription))
                 <+> parens (text (show pDimension))
  go !_ LineInput{lDescription,lDimension} = withBullet
    "PointInput" <+> doubleQuotes (text (T.unpack lDescription))
                 <+> parens (text (show lDimension))
  go !_ AreaInput{aDescription,aDimension} = withBullet
    "AreaInput" <+> doubleQuotes (text (T.unpack aDescription))
                <+> parens (text (show aDimension))
  go !_ (DimensionDependant _ dim) =
    "DimensionDependant" <+> (text (show dim))
  go !_ (Const d v) = "Constant" <+> text (show v) <+> parens (text (show d))
  go !n (s1 :<|> s2) =
    nextVar (goN n s1) $+$ ":<|>" $+$ nextVar (goN n s2)
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
  go !n (CheckPoint _ s2) =
    withBullet "CheckPoint" $$
      nextVar (goN n s2)
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
    text $ printf ":: %s (%s, %s)"
      (show (typeOf (undefined :: a)))
      (show (typeOf (undefined :: crs)))
      (show (typeOf (undefined :: dim)))


-- | Implementacion por defecto para mostrar cualquier 'Variable'
-- valida. Muestra su AST bonito.
instance IsVariable m t crs dim a => Show (Variable m t crs dim a)
  where show = show . prettyAST

instance HasDescription (Variable m t crs dim a) where
  description RasterInput {rDescription} = rDescription
  description PointInput {pDescription} = pDescription
  description LineInput {lDescription} = lDescription
  description AreaInput {aDescription} = aDescription
  description (Const _ v) = "Constant " <> fromString (show v)
  description (DimensionDependant _ _) = "Function of dimension"
  description (v :<|> w) = description v <> " or " <> description w
  description (Warp _ v) = "Warped " <> description v
  description (Grid _ v) = "Gridded " <> description v
  description (Rasterize _ v) = "Rasterized " <> description v
  description (Sample _ v w) = description w <> " sampled over " <> description v
  description (Aggregate _ v w) = description w <> " aggregated over " <> description v
  description (AdaptDim d _ w) = description w <> " adapted to " <> fromString (show d)
  description (CheckPoint _ w) = "CheckPoint for " <> description w
  description (Describe v _) = v
  description (Map _ v) = "Function of " <> description v
  description (ZipWith _ v w) = "Function of " <> description v <> " and " <> description w
