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
{-# LANGUAGE ViewPatterns #-}
module Sigym4.Data.Generic (
  Variable ((:<|>))
, RasterT
, AreaT
, LineT
, PointT

, IsVariable
, HasLoad (..)

, MissingInput (..)

, CanAdaptDim
, adaptDim

, Contourable
, contour

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

, describe

, Interpretable
, ofDimension
, IsInput
, input
, IsConst
, const
, foldDim
, mapReduce
, map
, zipWith

, getMissingInputs
, prettyAST
, dimension
, calculateFingerprint

) where

import           Sigym4.Data.AST as AST
import           Sigym4.Data.Fingerprint
import           Sigym4.Dimension

import           Control.Monad (foldM)
import           Control.Monad.Except (MonadError(catchError))
import           Data.Monoid ((<>))
import           Prelude hiding (const, map, zipWith)

          

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

-- | Crea isolineas a partir de un raster
--
contour
  :: Contourable m a
  => ContourSettings    m                  a
  -> Variable           m RasterT crs  dim a
  -> Variable           m LineT   crs  dim a
contour = Contour

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

-- | Una 'Variable' de entrada
input
  :: IsInput m t crs dim a
  => Loader m t crs dim a
  -> Variable m t crs dim a
input = Input

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


-- | Un left-fold sobre todos los indices dimensionales dada una funcion 'f',
-- un valor inicial (ie: (ix0,v0)) y una variable b.
--
-- Sirve para implementar variables recursivas (eg: canadiense, dias sin
-- precipitacion, etc)
foldDim
  :: ( Interpretable m t a
     , Interpretable m t b
     , IsVariable m t crs dim b
     )
  => WithFingerprint (Exp m a -> Exp m b -> Exp m a)
  -> (DimensionIx dim, Variable m t crs dim a)
  -> Variable m t crs dim b
  -> Variable m t crs dim a
foldDim = FoldDim

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
mapReduce
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
mapReduce = MapReduce

-- | Le da nombre a una 'Variable'. Unicamente sirve para consumo
-- humano. El interprete *debe* ignorarlo (ie: no usarlo para calculo
-- de 'Fingerprint's, nombres de tablas en base de datos, etc)
describe
  :: Description
  -> Variable m t crs dim a
  -> Variable m t crs dim a
describe = Describe

-- | Una entrada que falta con informacion asociada
data MissingInput = MissingInput
  { missingIx      :: SomeDimensionIx
  , missingDesc    :: Description
  , missingError   :: LoadError
  } deriving Show

-- | Devuelve una lista de 'MissingInput's con las entradas que
-- impiden que una 'Variable' se genere.
getMissingInputs
  :: IsVariable m t crs dim a
  => Variable m t crs dim a
  -> DimensionIx dim
  -> m [MissingInput]
getMissingInputs = go 100 [] where
  go
    :: forall m t crs dim a.  IsVariable m t crs dim a
    => Int
    -> [MissingInput]
    -> Variable m t crs dim a
    -> DimensionIx dim
    -> m [MissingInput]
  go !n z  _  _ | n<=0 = return z
  go !_ z (Input l) ix =
    (load l ix >> return z) `catchError` \e ->
      let mi = MissingInput
               (SomeDimensionIx (dimension l) ix)
               (description l) e
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

  go !n z (Contour _ v)     ix = go (n-1) z v ix
  go !n z (Warp _ v)        ix = go (n-1) z v ix
  go !n z (Grid _ v)        ix = go (n-1) z v ix
  go !n z (Rasterize _ v)   ix = go (n-1) z v ix

  go !n z (Sample    _ v w) ix = go (n-1) z v ix >>= \z' -> go (n-1) z' w ix
  go !n z (Aggregate _ v w) ix = go (n-1) z v ix >>= \z' -> go (n-1) z' w ix

  go !n z (Hoist w)           ix = hoist (go (n-1) z w ix)
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

  go !n z w@(FoldDim _ (ix0,v0) v) ix
    | ix<=ix0   = go (n-1) z v0 ix
    | otherwise = do
        let d = dimension v
            mqp = idpred d =<< idfloor d ix
        z' <- go (n-1) z v ix
        case mqp of
          Just (unQ -> p) -> go (n-1) z' w p
          Nothing         -> return z'

  go !n z (MapReduce _ _ _ s v) ix
    | null ixes = return (mi : z)
    | otherwise = foldM (\z' -> go (n-1) z' v) z ixes
    where mi = MissingInput
               (SomeDimensionIx (dimension v) ix)
               (description v)
               DimAdaptError 
          ixes = s ix

  go !n z (Describe   _ v)   ix    = go (n-1) z v ix
  go !n z (Map        _ v)   ix    = go (n-1) z v ix
  go !n z (ZipWith    _ v w) ix    = go (n-1) z v ix
                                 >>= \z' -> go (n-1) z' w ix
