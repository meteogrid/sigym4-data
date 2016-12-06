{-# LANGUAGE BangPatterns #-}
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
{-# LANGUAGE UndecidableInstances #-}
module Sigym4.Data.Generic (
  Variable ((:<|>))
, VariableType
, RasterT
, AreaT
, LineT
, PointT

, IsVariable
, IsRasterInput
, IsVectorInput

, MissingInput (..)

, CanAdaptDim
, adaptDim

, CanWarp
, warp

, CanGrid
, grid

, CanRasterize
, rasterize

, CanSample
, sample

, CanAggregate
, aggregate

, checkpoint
, describe

, Interpretable
, ofDimension
, const
, map
, zipWith

, getMissingInputs
, prettyAST
, dimension
, getFingerprint
, foldLeaves

) where

import           Sigym4.Data.AST as AST
import           Sigym4.Data.Fingerprint
import           Sigym4.Dimension

import           Control.Monad.Except (MonadError(catchError, throwError))
import           Data.Monoid ((<>))
import           Prelude hiding (const, map, zipWith)
import qualified Prelude as P

          

-- | Adapta las dimensiones de una variable.
--
--   Se usa, por ejemplo, para usar variables de observacion en
--   el calculo de una prediccion.
--
--   La funcion de adaptacion debe devolver una lista de posible
--   indices dimensionales que se probaran en orden.
--
--   La lista puede estar vacia si no hay adaptacion satisfactoria
--   en cuyo caso al intentarse cargar una variable devolvera
--   'DimAdaptError' lo cual semanticamente es lo mismo que
--   'NotAvailable'. Esto indica al interprete que explore otras
--   alternativas, si las hay.
adaptDim
  :: CanAdaptDim m t crs from to a
  => to
  -> (DimensionIx to -> [DimensionIx from])
  -> Variable m t crs from a
  -> Variable m t crs to a
adaptDim = AdaptDim

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
  :: CanWarp m t crs crs' dim a
  => WarpSettings m t          a
  -> Variable     m t crs' dim a
  -> Variable     m t crs  dim a
warp = Warp
  
-- | Interpola una variable de punto y produce una de raster.
--
--   Los puntos se reproyectan automaticamente al crs de salida
--   antes de interpolar
grid
  :: CanGrid m t crs crs' dim a
  => GridSettings  m t                a
  -> Variable      m t       crs' dim a
  -> Variable      m RasterT crs  dim a
grid = Grid

-- | Convierte una variable "Rasterizable" en una raster
--
--   La entrada se reproyecta automaticamente al crs de salida
rasterize
  :: CanRasterize m t crs crs' dim a
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
  :: CanSample m t crs crs' dim a b
  => SampleSettings       m t                 a
  -> Variable             m PointT   crs  dim b
  -> Variable             m t        crs' dim a
  -> Variable             m PointT   crs  dim a
sample = Sample


--
-- | Agrega valores bajo areas
--
--   Los poligonos de entrada se reproyectan automaticamente al crs
--   de entrada
aggregate
  :: CanAggregate m t crs crs' dim a b
  => AggregateSettings m t                a
  -> Variable          m AreaT   crs  dim b
  -> Variable          m t       crs' dim a
  -> Variable          m AreaT   crs  dim a
aggregate = Aggregate

-- | Una 'Variable' constante
const :: IsConst dim a => dim -> a -> Variable m t crs dim a
const = Const

-- | Aplica una funcion sobre todos los elementos
map
  :: ( IsVariable m t crs dim b
     , Interpretable m t a
     , Interpretable m t b
     )
  => WithFingerprint (Exp m b -> Exp m a)
  -> Variable m t crs dim b
  -> Variable m t crs dim a
map = Map

-- | Aplica una funcion binaria sobre todos los elementos
zipWith
  :: ( IsVariable m t crs dim b
     , IsVariable m t crs dim c
     , Interpretable m t a
     , Interpretable m t b
     , Interpretable m t c
     )
  => WithFingerprint (Exp m b -> Exp m c -> Exp m a)
  -> Variable m t crs dim b
  -> Variable m t crs dim c
  -> Variable m t crs dim a
zipWith = ZipWith


-- | Indica al interprete que se esfuerze en cachear una variable
-- cuando su indice dimensional pase el predicado.
--
-- Esto es util para variables recursivas (eg: canadiense, dias sin
-- precipitacion, etc..)
--
-- El interprete es libre de no hacerlo o de insertar nodos de estos
-- automaticamente si lo cree necesario.
checkpoint ::
  ( Dimension dim
  )
  => (DimensionIx dim -> Bool)
  -> Variable m t crs dim a
  -> Variable m t crs dim a
checkpoint = CheckPoint


-- | Le da nombre a una 'Variable'. Unicamente sirve para consumo
-- humano. El interprete *debe* ignorarlo (ie: no usarlo para calculo
-- de 'Fingerprint's, nombres de tablas en base de datos, etc)
describe
  :: Description
  -> Variable m t crs dim a
  -> Variable m t crs dim a
describe = Describe


-- | Devuelve la 'Dimension' de una 'Variable'
dimension :: Variable m t crs dim a -> dim
dimension RasterInput{rDimension}  = rDimension
dimension PointInput{pDimension}   = pDimension
dimension LineInput{lDimension}    = lDimension
dimension AreaInput{aDimension}    = aDimension
-- Para poder escribir la siguiente ecuacion es el unico motivo por el
-- cual Const lleva dimension asociada.
dimension (Const d _)              = d
dimension (DimensionDependant _ d) = d
-- Esto es dudoso... cual es la dimension de una alternativa?
-- Ahora mismo no podemos decir "la de la que se seleccione" porque
-- esto es una funcion pura que no depende de si se puede generar o
-- no.
-- Creo (AVG) que es "moralmente correcto" decir que la de la opcion
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



-- | Calcula la huella ('Fingerprint') de una variable.
--
--   Se garantiza que si la huella de una variable no ha cambiado
--   el contenido tampoco.
--
--   El interprete *debe* calcular esto eficientemente para las
--   entradas, cacheando agresivamente si puede ya que esto se
--   hace muy a menudo.
--
--   La huella de las variables derivadas siempre se puede
--   calcular sin calcular las variables en si
--   por lo que es muy barato saber si hay que regenerar algo.
--   Ya sea por cambio en las entradas (eg: llegan nuevas
--   observaciones de estacion, llega fichero corregido tras envio
--   de fichero corrupto, etc) o por cambios en el codigo
--   (asumiendo que el 'Fingerprint' de las funciones envueltas con 
--   'WithFingerprint' se genere correctamente).
getFingerprint
  :: MonadError LoadError m
  => Variable m t crs dim a
  -> DimensionIx dim
  -> m Fingerprint

-- La huella de las entradas es la calculada por el cargador
getFingerprint RasterInput{rFingerprint}  = rFingerprint
getFingerprint PointInput{pFingerprint}   = pFingerprint
getFingerprint LineInput{lFingerprint}    = lFingerprint
getFingerprint AreaInput{aFingerprint}    = aFingerprint

-- La de una constante es la de su valor. La dimension no nos importa.
-- (Asumimos que se calcula muy rapido)
getFingerprint (Const _ v) =  P.const . return $ fingerprint v

-- La de una funcion del indice dimensional es funcion de su resultado
-- (Asumimos que se calcula muy rapido)
getFingerprint (DimensionDependant f _)   = return . fingerprint . f

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
getFingerprint (v :<|> w) = \ix ->
  getFingerprint v ix `catchError` (\_ -> getFingerprint w ix)

-- La huella de las operaciones intrinsecas es la huella de las
-- variables de entrada combinada con la de la de su configuracion.
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
--      valida, es decir, que "se porta bien".
--
getFingerprint (AdaptDim _ fun v) = \ix ->
  let loop (x:xs) = getFingerprint v x `catchError` P.const (loop xs)
      loop []     = throwError DimAdaptError

  in loop (fun ix)

getFingerprint (CheckPoint _ v) = getFingerprint v
getFingerprint (Describe   _ v) = getFingerprint v

-- La huella de la aplicaciones es la huella de la funcion combinada
-- con la de sus entradas.
getFingerprint (Map f v)  = combineVarFPWith v f
getFingerprint (ZipWith f v w) = combineVarsFPWith v w f

combineVarFPWith
  :: (HasFingerprint o, MonadError LoadError m)
  => Variable m t crs dim a
  -> o
  -> DimensionIx dim
  -> m Fingerprint
combineVarFPWith v o ix = do
  fv <- getFingerprint v ix
  return (fv <> fingerprint o)

combineVarsFPWith
  :: (HasFingerprint o, MonadError LoadError m)
  => Variable m t crs dim a
  -> Variable m t' crs' dim a'
  -> o
  -> DimensionIx dim
  -> m Fingerprint
combineVarsFPWith v w o ix = do
  fv <- getFingerprint v ix
  fw <- combineVarFPWith w o ix
  return (mconcat [fingerprint o, fv, fw])

-- | Recorre todas las hojas del AST de la misma dimension en
-- pre-orden.
--
-- No desciende en los nodos AdaptDim porque al no existir
-- adaptacion automatica de dimensiones no sabria por donde bajar.
-- Es responsabilidad de la funcion de reduccion descender si puede
foldLeaves
  :: forall m t crs dim a b.
     ( Monad m, IsVariable m t crs dim a )
  => (forall t' crs' a'. IsVariable m t' crs' dim a'
        => b -> Variable m t' crs' dim a' -> m b)
  -> b
  -> Variable m t crs dim a
  -> m b
foldLeaves f = go maxDepth where
  maxDepth = 100 :: Int

  go :: forall t' crs' a'.  IsVariable m t' crs' dim a'
     => Int
     -> b
     -> Variable m t' crs' dim a'
     -> m b
  go !n z _               | n==0 = return z
  go !_ z v@RasterInput{}        = f z v
  go !_ z v@PointInput{}         = f z v
  go !_ z v@LineInput{}          = f z v
  go !_ z v@AreaInput{}          = f z v
  go !_ z v@Const{}              = f z v
  go !_ z v@DimensionDependant{} = f z v
  go !n z (w :<|> w')            = go (n-1) z w
                               >>= flip (go (n-1)) w'
  go !n z (Warp _ w)             = go (n-1) z w
  go !n z (Grid _ w)             = go (n-1) z w
  go !n z (Rasterize _ w)        = go (n-1) z w
  go !n z (Sample _ w w')        = go (n-1) z w
                               >>= flip (go (n-1)) w'
  go !n z (Aggregate _ w w')     = go (n-1) z w
                               >>=  flip (go (n-1)) w'
  go !_ z v@AdaptDim{}           = f z v
  go !n z (CheckPoint _ w)       = go (n-1) z w
  go !n z (Describe   _ w)       = go (n-1) z w
  go !n z (Map _ w)              = go (n-1) z w
  go !n z (ZipWith _ w w')       = go (n-1) z w
                               >>= flip (go (n-1)) w'

-- | Una entrada que falta con informacion asociada
data MissingInput = MissingInput
  { missingIx      :: SomeDimensionIx
  , missingDesc    :: Description
  , missingError   :: LoadError
  } deriving Show

-- | Devuelve una lista de 'MissingInput's con las entradas que
-- impiden que una 'Variable' se genere.
getMissingInputs
  :: (Monad m, IsVariable m t crs dim a)
  => Variable m t crs dim a -> DimensionIx dim
  -> m [MissingInput]
getMissingInputs = go 100 [] where
  go
    :: forall m t crs dim a.
       (Monad m, IsVariable m t crs dim a)
    => Int
    -> [MissingInput]
    -> Variable m t crs dim a
    -> DimensionIx dim
    -> m [MissingInput]
  go !n z  _  _ | n<=0 = return z
  go !_ z RasterInput{rLoad,rDescription,rDimension} ix =
    (rLoad ix >> return z)  `catchError` \e ->
      let mi = MissingInput
               (SomeDimensionIx rDimension ix)
               rDescription e
      in return (mi : z)
    
  go !_ z PointInput{pLoad,pDescription,pDimension} ix =
    (pLoad ix >> return z)  `catchError` \e ->
      let mi = MissingInput
               (SomeDimensionIx pDimension ix)
               pDescription e
      in return (mi : z)

  go !_ z LineInput{lLoad,lDescription,lDimension} ix =
    (lLoad ix >> return z)  `catchError` \e ->
      let mi = MissingInput
               (SomeDimensionIx lDimension ix)
               lDescription e
      in return (mi : z)

  go !_ z AreaInput{aLoad,aDescription,aDimension} ix =
    (aLoad ix >> return z)  `catchError` \e ->
      let mi = MissingInput
               (SomeDimensionIx aDimension ix)
               aDescription e
      in return (mi : z)

  -- Que bien, estas nunca faltan
  go !_ z Const{}              _ = return z
  go !_ z DimensionDependant{} _ = return z

  -- Las entradas que faltan en una alternativa son las primeras que
  -- falten, no todas las que faltan
  go !n z (v :<|> w) ix = do
    z' <- go (n-1) [] v ix
    if null z'
      -- Si el acumulador ha quedado igual es que no falta nada en la
      -- opcion principal. No calculamos las alternativas porque si no
      -- siempre (o casi siempre) faltaria algo
      then return z
      else go (n-1) (z <> z') w ix

  go !n z (Warp _ v)        ix = go (n-1) z v ix
  go !n z (Grid _ v)        ix = go (n-1) z v ix
  go !n z (Rasterize _ v)   ix = go (n-1) z v ix

  go !n z (Sample    _ v w) ix = go (n-1) z v ix >>= \z' -> go (n-1) z' w ix
  go !n z (Aggregate _ v w) ix = go (n-1) z v ix >>= \z' -> go (n-1) z' w ix

  go !n z ad@(AdaptDim d f v) ix =
    case f ix of
      [] -> -- Si no hay adaptacion razonable de dimensiones marcamos el
            -- nodo AdaptDim como entrada que falta. Esto permite que
            -- se visiten las alternativas en caso de haberlas
            let mi = MissingInput (SomeDimensionIx d ix)
                                  (description ad)
                                  DimAdaptError 
            in return (mi : z)
      xs@(x:_) -> loop xs x
        where
          loop [] ix' =
            -- Fallan todas las opciones, decimos que falta
            -- la variable adaptada con el ultimo indice
            -- probado
            let mi = MissingInput
                  (SomeDimensionIx (dimension v) ix')
                  (description v) DimAdaptError 
            in return (mi : z)
          loop (ix':xs') _  = do
            z' <- go (n-1) [] v ix'
            if null z'
              then -- hay una opcion posible, decimos que no falta nada 
                return []
              else -- seguimos probando
                loop xs' ix'

  go !n z (CheckPoint _ v)   ix = go (n-1) z v ix
  go !n z (Describe   _ v)   ix = go (n-1) z v ix
  go !n z (Map        _ v)   ix = go (n-1) z v ix
  go !n z (ZipWith    _ v w) ix = go (n-1) z v ix
                                          >>= \z' -> go (n-1) z' w ix
