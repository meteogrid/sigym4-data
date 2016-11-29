{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Sigym4.Data.Generic (
  Variable ((:<|>))
, VariableType (..)
, MissingInput (..)
, RasterT
, AreaT
, PointT

-- * Constructores de nodos del AST y sus restricciones
, CanAdaptDim
, adaptDim

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

-- * Utilidades varias
, getMissingInputs
, prettyAST
, dimension
, getFingerprint
, isInput
, isDerived

-- * Utilidades semi-internas para analisis de AST
, foldAST
) where

import           Sigym4.Data.AST as AST
import           Sigym4.Data.Fingerprint
import           Sigym4.Data.Units
import           Sigym4.Dimension
import           SpatialReference

import qualified Data.Text as T
import           Data.Monoid ((<>))
import           Data.Typeable ( Typeable, typeOf )
import           Text.PrettyPrint hiding ((<>))
import           Prelude hiding (map, zipWith)

          

-- | Adapta las dimensiones de una variable.
--
--   Se usa, por ejemplo, para usar variables de observacion en
--   el calculo de una prediccion. La funcion de adaptacion
--   debe devolver una lista no vacia de posibles indices que se
--   probaran en orden
adaptDim
  :: CanAdaptDim m t crs from to a
  => to
  -> (from -> DimensionIx to -> [DimensionIx from])
  -> Variable m t crs from a
  -> Variable m t crs to a
adaptDim = AdaptDim

-- | Es una Variable una entrada que no sabemos generar?
isInput :: Variable m t crs to a -> Bool
isInput RasterInput{}        = True
isInput PointInput{}         = True
isInput AreaInput{}          = True
isInput DimensionDependant{} = True
isInput _             = False

-- | Es una Variable una derivada?
isDerived :: Variable m t crs to a -> Bool
isDerived = not . isInput

-- | Una variable que solo depende de la dimension
ofDimension
  :: (HasFingerprint a, Dimension dim, Show dim)
  => (DimensionIx dim -> a) -> dim
  -> Variable m t crs dim a
ofDimension = DimensionDependant

-- | Reproyecta una entrada.
--
--   Tambien sirve para especificar un algoritmo de resampleo entre
--   rasters del mismo sistema de referencia si no se quiere que se
--   use el algoritmo de resampleo por defecto (vecino mas cercano)
--   para adaptar distintas resoluciones.
warp
  :: ( KnownCrs crs
     , KnownCrs crs'
     , Warpable m t a
     , Typeable (Variable m t crs' dim a)
     )
  => WarpSettings m t          a
  -> Variable     m t crs' dim a
  -> Variable     m t crs  dim a
warp = Warp
  
-- | Interpola una variable de punto y produce una de raster.
--
--   Los puntos se reproyectan automaticamente al crs de salida
--   antes de interpolar
grid
  :: ( KnownCrs crs
     , KnownCrs crs'
     , Griddable m t a
     , Typeable (Variable m t crs' dim a)
     )
  => GridSettings  m t                a
  -> Variable      m t       crs' dim a
  -> Variable      m RasterT crs  dim a
grid = Grid

-- | Convierte una variable "Rasterizable" en una raster
--
--   La entrada se reproyecta automaticamente al crs de salida
rasterize
  :: ( KnownCrs crs
     , KnownCrs crs'
     , Rasterizable m t a
     , Typeable (Variable m t crs' dim a)
     )
  => RasterizeSettings  m t                a
  -> Variable           m t       crs' dim a
  -> Variable           m RasterT crs  dim a
rasterize = Rasterize


--
-- | Produce variables de punto a partir de otros
--   tipos de variable
--
--   Los puntos de entrada se reproyectan automaticamente al crs
--   de entrada. Para Tener mas control usar Warp
sample
  :: ( KnownCrs crs
     , KnownCrs crs'
     , Sampleable m t a
     , Typeable (Variable m t crs' dim a)
     )
  => SampleSettings       m t                 a
  -> Variable             m PointT   crs  dim any
  -> Variable             m t        crs' dim a
  -> Variable             m PointT   crs  dim a
sample = Sample


--
-- | Agrega valores bajo areas
--
--   Los poligonos de entrada se reproyectan automaticamente al crs
--   de entrada
aggregate
  :: ( KnownCrs crs
     , KnownCrs crs'
     , Aggregable m t a
     , Typeable (Variable m t crs' dim a)
     )
  => AggregateSettings m t                a
  -> Variable          m AreaT   crs  dim any
  -> Variable          m t       crs' dim a
  -> Variable          m AreaT   crs  dim a
aggregate = Aggregate

-- | Aplica una funcion sobre todos los elementos
map
  :: ( HasUnits b
     , Typeable (Variable m t crs dim b)
     )
  => WithFingerprint (Exp m b -> Exp m a)
  -> Variable m t crs dim b
  -> Variable m t crs dim a
map = Map

-- | Aplica una funcion binaria sobre todos los elementos
zipWith
  :: ( HasUnits b
     , HasUnits c
     , Typeable (Variable m t crs dim b)
     , Typeable (Variable m t crs dim c)
     )
  => WithFingerprint (Exp m b -> Exp m c -> Exp m a)
  -> Variable m t crs dim b
  -> Variable m t crs dim c
  -> Variable m t crs dim a

zipWith = ZipWith


-- | Indica al interprete que se esfuerze en cachear una variable
--   cuando su indice dimensional pase el predicado.
--
--   Esto es util para variables recursivas (eg: canadiense, dias sin
--   precipitacion, etc..)
--
--   El interprete es libre de no hacerlo o de insertar nodos de estos
--   automaticamente si lo cree necesario.
--
checkpoint ::
  ( Dimension dim
  )
  => (DimensionIx dim -> Bool)
  -> Variable m t crs dim a
  -> Variable m t crs dim a
checkpoint = CheckPoint


describe
  :: Description
  -> Variable m t crs dim a
  -> Variable m t crs dim a
describe = Describe


-- | Devuelve la dimension de una variable
dimension :: Variable m t crs dim a -> dim
dimension RasterInput{rDimension}  = rDimension
dimension PointInput{pDimension}   = pDimension
dimension AreaInput{aDimension}    = aDimension
dimension (DimensionDependant _ d) = d
-- Esto es dudoso... cual es la dimension de una alternativa?
-- Ahora mismo no podemos decir "la de la que se seleccione" porque
-- esto es una funcion pura que no depende de si se puede generar o
-- no.
-- Creo (AVG) que es "moralmente" correcto decir que la de la opcion
-- ideal
dimension (v :<|> _)               = dimension v
dimension (Warp _ v)               = dimension v
dimension (Grid _ v)               = dimension v
dimension (Rasterize _ v)          = dimension v
dimension (Sample _ _ v)           = dimension v
dimension (Aggregate _ _ v)        = dimension v
dimension (AdaptDim d _ _)         = d
dimension (CheckPoint _ v)         = dimension v
dimension (Describe _ v)           = dimension v
dimension (Map _ v)                = dimension v
-- En las aplicaciones de mas de una variable cogemos la dimension
-- de la primera variable siempre. Si se necesita mas control se
-- puede envolver con adaptDim
dimension (ZipWith _ v _)          = dimension v



-- | Devuelve la huella de una variable.
--
--   Se garantiza que si la huella de una variable no ha cambiado
--   el contenido tampoco.
--
--   La huella de las variables derivadas siempre se puede
--   calcular sin calcular las variables en si
--   por lo que es muy barato saber si hay que regenerar algo.
--   Ya sea por cambio en las entradas (eg: llegan nuevas
--   observaciones de estacion, llega fichero corregido tras envio
--   de fichero corrupto, etc) o por cambios en el codigo
--   (asumiendo que el Fingerprint de las funciones envueltas con 
--   WithFingerprint se genere correctamente).
--
getFingerprint
  :: Monad m
  => Variable m t crs dim a
  -> DimensionIx dim
  -> m (Either LoadError Fingerprint)

-- La huella de las entradas es la calculada por el cargador
getFingerprint RasterInput{rFingerprint}  = rFingerprint
getFingerprint PointInput{pFingerprint}   = pFingerprint
getFingerprint AreaInput{aFingerprint}    = aFingerprint

-- La de una funcion del indice dimensional es producto de su resultado
-- (Asumimos que se calcula muy rapido)
getFingerprint (DimensionDependant f _)   =
  return . Right . fingerprint . f

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
--  Es decir, que se "porta bien".
--
getFingerprint (v :<|> w)               = \ix ->
  getFingerprint v ix
  >>= either (const (getFingerprint w ix)) (return . Right)

-- La huella de las operaciones "builtin" es la huella de las
-- variables de entrada y de su configuracion.
getFingerprint (Warp s v)               = combineVarFPWith v s
getFingerprint (Grid s v)               = combineVarFPWith v s
getFingerprint (Rasterize s v)          = combineVarFPWith v s
getFingerprint (Sample s v w)           = combineVarsFPWith v w s
getFingerprint (Aggregate s v w)        = combineVarsFPWith v w s
--
-- La huella de una adaptacion de dimension es la huella del
-- primer indice adaptado que devuelva huella
--
-- OJO: Asume que el interprete realmente ejecuta la primera opcion
--      valida, es decir, que se "porta bien".
--
getFingerprint va@(AdaptDim dim fun v) = \ix ->
  let loop (x:xs) = do efv <- getFingerprint v x
                       either (const (loop xs)) (return . Right) efv
      loop [] = return $
        Left (DimAdaptError (varDescription va) (SomeDimensionIx dim ix))
      varDescription = error "TBD!"

  in loop (fun (dimension v) ix)

getFingerprint (CheckPoint _ v) = getFingerprint v
getFingerprint (Describe   _ v) = getFingerprint v

-- La huella de la aplicacion de una funcion unaria es la huella
-- de la variable de entrada combinada con la de adaptacion
getFingerprint (Map f v)  = combineVarFPWith v f
getFingerprint (ZipWith f v w) = combineVarsFPWith v w f

combineVarFPWith
  :: (HasFingerprint o, Monad m)
  => Variable m t crs dim a
  -> o
  -> DimensionIx dim
  -> m (Either LoadError Fingerprint)
combineVarFPWith v o ix = do
  efv <- getFingerprint v ix
  let fo = [fingerprint o]
  return (either Left (Right . mconcat . (:fo)) efv)

combineVarsFPWith
  :: (HasFingerprint o, Monad m)
  => Variable m t crs dim a
  -> Variable m t' crs' dim a'
  -> o
  -> DimensionIx dim
  -> m (Either LoadError Fingerprint)
combineVarsFPWith v w o ix = do
  efv <- getFingerprint v ix
  case efv of
    Left _ -> return efv
    Right fv -> do
      efw <- combineVarFPWith w o ix
      return $ case efw of
        Left _ -> efw
        Right fw -> Right (mconcat [fingerprint o, fv, fw])

class (Monad m, Monad m') => Hoistable m m' where
  hoist :: forall a. m' a -> m a

instance (Monad m, m ~ m') => Hoistable m m' where
  hoist = id

-- | Recorre todos los nodos del AST de la misma dimension en pre-orden 
--   (ie: procesa primero a los padres y luego a los hijos
--   de izquierda a derecha)
--   No desciende en los nodos AdaptDim porque no puede probar que el
--   indice dimensional es del mismo tipo. Es responsabilidad de la
--   funcion de reduccion descender si puede
--   (eg: getMissingInputs lo hace)
foldAST
  :: forall m t crs dim a b. Hoistable m m
  => (forall m' t' dim' crs' a'.
      ( Hoistable m m', DimensionIx dim ~ DimensionIx dim' )
      => b -> Variable m' t' crs' dim' a' -> m' b
     )
  -> b
  -> Variable m t crs dim a
  -> m b
foldAST f = go where
  go :: forall m' t' crs' a'. Hoistable m m'
     => b
     -> Variable m' t' crs' dim a'
     -> m b
  go z v@RasterInput{}        = hoist (f z v)
  go z v@PointInput{}         = hoist (f z v)
  go z v@AreaInput{}          = hoist (f z v)
  go z v@DimensionDependant{} = hoist (f z v)
  go z v@(w :<|> w')          = hoist (f z v >>= flip f w >>= flip f w')
  go z v@(Warp _ w)           = hoist (f z v >>= flip f w)
  go z v@(Grid _ w)           = hoist (f z v >>= flip f w)
  go z v@(Rasterize _ w)      = hoist (f z v >>= flip f w)
  go z v@(Sample _ w w')      = hoist (f z v >>= flip f w >>= flip f w')
  go z v@(Aggregate _ w w')   = hoist (f z v >>= flip f w >>= flip f w')
  go z v@AdaptDim{}           = hoist (f z v)
  go z v@(CheckPoint _ w)     = hoist (f z v >>= flip f w)
  go z v@(Describe   _ w)     = hoist (f z v >>= flip f w)
  go z v@(Map _ w)            = hoist (f z v >>= flip f w)
  go z v@(ZipWith _ w w')     = hoist (f z v >>= flip f w >>= flip f w')

data MissingInput = MissingInput
  { missingIx      :: SomeDimensionIx
  , missingDesc    :: Description
  , missingError   :: LoadError
  } deriving Show

-- | Devuelve una lista con las descripciones de las entradas que no
--   se pueden generar
getMissingInputs
  :: forall m t crs dim a. Monad m
  => Variable m t crs dim a -> DimensionIx dim
  -> m [MissingInput]
getMissingInputs v0 ix = foldAST step [] v0 where

  step :: forall m' t' crs' dim' a'.
          ( Hoistable m m', DimensionIx dim ~  DimensionIx dim' )
      => [MissingInput]
      -> Variable m' t' crs' dim' a'
      -> m' [MissingInput]

  step z RasterInput{rLoad,rDescription,rDimension} = do
    r <- hoist (rLoad ix)
    return $ case r of
      Right _ -> z
      Left e  ->
        let mi = MissingInput (SomeDimensionIx rDimension ix) rDescription e
        in mi : z
    
  step z PointInput{pLoad,pDescription,pDimension} = do
    r <- hoist (pLoad ix)
    return $ case r of
      Right _ -> z
      Left e  ->
        let mi = MissingInput (SomeDimensionIx pDimension ix) pDescription e
        in mi : z

  step z AreaInput{aLoad,aDescription,aDimension} = do
    r <- hoist (aLoad ix)
    return $ case r of
      Right _ -> z
      Left e  ->
        let mi = MissingInput (SomeDimensionIx aDimension ix) aDescription e
        in mi : z

  step z DimensionDependant{} = return z

  -- Las entradas que faltan en una alternativa son las primeras que
  -- falten, no todas las que faltan
  step z (v :<|> w) = do
    z' <- step [] v
    if null z'
      -- Si el acumulador ha quedado igual es que no falta nada en la
      -- opcion principal. No calculamos las alternativas porque si no
      -- siempre (o casi siempre) faltaria algo
      then return z
      else step (z <> z') w

  step z (Warp _ v) = step z v
  step z (Grid _ v) = step z v
  step z (Rasterize _ v) = step z v

  step z (Sample    _ v w) = step z v >>= flip step w
  step z (Aggregate _ v w) = step z v >>= flip step w

  step z (AdaptDim _ f v) =
    let loop z' (x:xs) = do
          z'' <- hoist (getMissingInputs v x)
          if null z'' then return z' else loop z'' xs
        loop z' [] = return z'
    in loop z (f (dimension v) ix)
  step z (CheckPoint _ v) = step z v
  step z (Describe   _ v) = step z v

  step z (Map     _ v  ) = step z v
  step z (ZipWith _ v w) = step z v >>= flip step w

prettyAST
  :: forall m t crs dim a. Typeable (Variable m t crs dim a)
  => Variable m t crs dim a -> Doc
prettyAST = goV where
  go, goV :: Variable m t crs dim a -> Doc
  goV v = text (show (typeOf v)) $$ nest 2 (go v) where
  go RasterInput{rDescription,rDimension} =
    "RasterInput" <+> doubleQuotes (text (T.unpack rDescription))
                  <+> parens (text (show rDimension))
  go PointInput{pDescription,pDimension} =
    "PointInput" <+> doubleQuotes (text (T.unpack pDescription))
                 <+> parens (text (show pDimension))
  go AreaInput{aDescription,aDimension} =
    "AreaInput" <+> doubleQuotes (text (T.unpack aDescription))
                <+> parens (text (show aDimension))
  go (DimensionDependant _ dim) =
    "DimensionDependant" <+> (text (show dim))
  go (s1 :<|> s2) =
    nest 2 (goV s1) <+> ":<|>" $$ nest 2 (goV s2)
  go (Warp s1 s2) =
    "Warp" <+> text (show s1) $$ nest 2 (prettyAST s2)
  go (Grid s1 s2) =
    "Grid" <+> text (show s1) $$ nest 2 (prettyAST s2)
  go (Rasterize s1 s2) =
    "Rasterize" <+> text (show s1) $$ nest 2 (prettyAST s2)
  go (Sample s1 _ s2) =
    "Sample" <+> text (show s1) $$ nest 2 (prettyAST s2)
  go (Aggregate s1 _ s2) =
    "Aggregate" <+> text (show s1) $$ nest 2 (prettyAST s2)
  go (AdaptDim dim _ s2) =
    "AdaptDim" <+> text (show dim) $$ nest 2 (prettyAST s2)
  go (CheckPoint _ s2) =
    "CheckPoint" $$ nest 2 (prettyAST s2)
  go (Describe desc var) =
    text (T.unpack desc) $$ nest 2 (prettyAST var)
  go (Map _ s2) =
    "Map" $$ nest 2 (prettyAST s2)
  go (ZipWith _ a b) =
    "ZipWith" $$ nest 2 (prettyAST a) $$ nest 2 (prettyAST b)
