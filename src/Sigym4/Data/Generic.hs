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
, RasterT
, AreaT
, PointT

-- * Constructores de nodos del AST
, adaptDim
, dimension
, fingerprint
, isInput
, isDerived
, ofDimension
, warp
, grid
, rasterize
, sample
, aggregate
, checkpoint
, describe
, Sigym4.Data.Generic.map

-- * Utilidades varias
, getMissingInputs
, prettyAST

-- * Utilidades semi-internas para analisis de AST
, foldAST
) where

import qualified Sigym4.Data.AST as AST
import           Sigym4.Data.AST hiding (fingerprint)
import           Sigym4.Data.Units
import           Sigym4.Dimension
import           SpatialReference

import           Crypto.Hash
import qualified Data.Text as T
import           Data.List.NonEmpty (NonEmpty, toList)
import           Data.Monoid ((<>))
import           Data.Typeable ( Typeable, typeOf )
import           Text.PrettyPrint hiding ((<>))

          

-- | Adapta las dimensiones de una variable.
--
--   Se usa, por ejemplo, para usar variables de observacion en
--   el calculo de una prediccion. La funcion de adaptacion
--   debe devolver una lista no vacia de posibles indices que se
--   probaran en orden
adaptDim
  :: ( Dimension from
     , Dimension to
     , Show to
     , Typeable (Variable m exp t crs from a)
     )
  => to
  -> WithFingerprint (from -> DimensionIx to -> NonEmpty (DimensionIx from))
  -> Variable m exp t crs from a
  -> Variable m exp t crs to a
adaptDim = AdaptDim

-- | Es una Variable una entrada que no sabemos generar?
isInput :: Variable m exp t crs to a -> Bool
isInput RasterInput{}        = True
isInput PointInput{}         = True
isInput AreaInput{}          = True
isInput DimensionDependant{} = True
isInput _             = False

-- | Es una Variable una derivada?
isDerived :: Variable m exp t crs to a -> Bool
isDerived = not . isInput

-- | Una variable que solo depende de la dimension
ofDimension
  :: (HasFingerprint (DimensionIx dim), Dimension dim, Show dim)
  => WithFingerprint (DimensionIx dim -> a) -> dim
  -> Variable m exp t crs dim a
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
     , Warpable m exp t a
     , Typeable (Variable m exp t crs' dim a)
     )
  => WarpSettings m exp t          a
  -> Variable     m exp t crs' dim a
  -> Variable     m exp t crs  dim a
warp = Warp
  
-- | Interpola una variable de punto y produce una de raster.
--
--   Los puntos se reproyectan automaticamente al crs de salida
--   antes de interpolar
grid
  :: ( KnownCrs crs
     , KnownCrs crs'
     , Griddable m exp t a
     , Typeable (Variable m exp t crs' dim a)
     )
  => GridSettings  m exp t                a
  -> Variable      m exp t       crs' dim a
  -> Variable      m exp RasterT crs  dim a
grid = Grid

-- | Convierte una variable "Rasterizable" en una raster
--
--   La entrada se reproyecta automaticamente al crs de salida
rasterize
  :: ( KnownCrs crs
     , KnownCrs crs'
     , Rasterizable m exp t a
     , Typeable (Variable m exp t crs' dim a)
     )
  => RasterizeSettings  m exp t                a
  -> Variable           m exp t       crs' dim a
  -> Variable           m exp RasterT crs  dim a
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
     , Sampleable m exp t a
     , Typeable (Variable m exp t crs' dim a)
     )
  => SampleSettings       m exp t                 a
  -> Variable             m exp PointT   crs  dim any
  -> Variable             m exp t        crs' dim a
  -> Variable             m exp PointT   crs  dim a
sample = Sample


--
-- | Agrega valores bajo areas
--
--   Los poligonos de entrada se reproyectan automaticamente al crs
--   de entrada
aggregate
  :: ( KnownCrs crs
     , KnownCrs crs'
     , Aggregable m exp t a
     , Typeable (Variable m exp t crs' dim a)
     )
  => AggregateSettings m exp t                a
  -> Variable          m exp AreaT   crs  dim any
  -> Variable          m exp t       crs' dim a
  -> Variable          m exp AreaT   crs  dim a
aggregate = Aggregate

-- | Aplica una funcion sobre todos los elemementos
map
  :: ( HasUnits b
     , Typeable (Variable m exp t crs dim b)
     )
  => WithFingerprint (exp b -> exp a)
  -> Variable m exp t crs dim b
  -> Variable m exp t crs dim a
map = Map


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
  -> Variable m exp t crs dim a
  -> Variable m exp t crs dim a
checkpoint = CheckPoint


describe
  :: Description
  -> Variable m exp t crs dim a
  -> Variable m exp t crs dim a
describe = Describe


-- | Devuelve la dimension de una variable
dimension :: Variable m exp t crs dim a -> dim
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
fingerprint :: Monad m
            => Variable m exp t crs dim a
            -> DimensionIx dim
            -> m (Either LoadError Fingerprint)

-- La huella de las entradas es la calculada por el cargador
fingerprint RasterInput{rFingerprint}  = rFingerprint
fingerprint PointInput{pFingerprint}   = pFingerprint
fingerprint AreaInput{aFingerprint}    = aFingerprint

-- La de una funcion del indice dimensional es producto de
-- la huella de la funcion y de la huella del indice
fingerprint (DimensionDependant f _)   =
  return . Right . combineFPs . (:AST.fingerprint f:[]) . AST.fingerprint

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
fingerprint (v :<|> w)               = \ix ->
  fingerprint v ix
  >>= either (const (fingerprint w ix)) (return . Right)

-- La huella de las operaciones "builtin" es la huella de las
-- variables de entrada y de su configuracion.
fingerprint (Warp s v)               = combineVarFPWith v s
fingerprint (Grid s v)               = combineVarFPWith v s
fingerprint (Rasterize s v)          = combineVarFPWith v s
fingerprint (Sample s v w)           = combineVarsFPWith v w s
fingerprint (Aggregate s v w)        = combineVarsFPWith v w s
--
-- La huella de una adaptacion de dimension es la huella del
-- primer indice adaptado que devuelva huella combinado con la
-- huella de la funcion de adaptacion
--
-- OJO: Asume que el interprete realmente ejecuta la primera opcion
--      valida, es decir, que se "porta bien".
--
fingerprint (AdaptDim _ (WithFingerprint fp fun) v) = \ix -> do
  let loop (x:[]) = fingerprint v x
      loop (x:xs) = do efv <- fingerprint v x
                       either (const (loop xs)) (return . Right) efv
      -- Unreachable ya que la funcion de adaptacion devuelve
      -- NonEmpty
      loop [] = error "unreachable"
  efv <- loop (toList (fun (dimension v) ix))
  case efv of
    Right fv -> return (Right (combineFPs [fv, fp]))
    Left _   -> return efv

fingerprint (CheckPoint _ v) = fingerprint v
fingerprint (Describe   _ v) = fingerprint v

-- La huella de la aplicacion de una funcion unaria es la huella
-- de la variable de entrada combinada con la de adaptacion
fingerprint (Map f v) = combineVarFPWith v f

combineFPs
  :: [Fingerprint] -> Fingerprint
combineFPs = hashFinalize . hashUpdates hashInit

combineVarFPWith
  :: (HasFingerprint o, Monad m)
  => Variable m exp t crs dim a
  -> o
  -> DimensionIx dim
  -> m (Either LoadError Fingerprint)
combineVarFPWith v o ix = do
  efv <- fingerprint v ix
  let fo = [AST.fingerprint o]
  return (either Left (Right . combineFPs . (:fo)) efv)

combineVarsFPWith
  :: (HasFingerprint o, Monad m)
  => Variable m exp t crs dim a
  -> Variable m exp' t' crs' dim a'
  -> o
  -> DimensionIx dim
  -> m (Either LoadError Fingerprint)
combineVarsFPWith v w o ix = do
  efv <- fingerprint v ix
  case efv of
    Left _ -> return efv
    Right fv -> do
      efw <- combineVarFPWith w o ix
      return $ case efw of
        Left _ -> efw
        Right fw -> Right (combineFPs [AST.fingerprint o, fv, fw])

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
  :: forall m exp t crs dim a b. Hoistable m m
  => (forall m' exp' t' dim' crs' a'.
      ( Hoistable m m', DimensionIx dim ~ DimensionIx dim' )
      => b -> Variable m' exp' t' crs' dim' a' -> m' b
     )
  -> b
  -> Variable m exp t crs dim a
  -> m b
foldAST f = go where
  go :: forall m' exp' t' crs' a'. Hoistable m m'
     => b
     -> Variable m' exp' t' crs' dim a'
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

data SomeDimensionIx where
  SomeDimensionIx :: ( Show (DimensionIx dim)
                     , Show dim
                     , Dimension dim
                     )
                  => dim -> DimensionIx dim -> SomeDimensionIx
deriving instance Show SomeDimensionIx

-- | Devuelve una lista con las descripciones de las entradas que no
--   se pueden generar
getMissingInputs
  :: forall m exp t crs dim a. Monad m
  => Variable m exp t crs dim a -> DimensionIx dim
  -> m [(SomeDimensionIx, Description, Message)]
getMissingInputs v0 ix = foldAST step [] v0 where

  step :: forall m' exp' t' crs' dim' a'.
          ( Hoistable m m', DimensionIx dim ~  DimensionIx dim' )
      => [(SomeDimensionIx, Description, Message)]
      -> Variable m' exp' t' crs' dim' a'
      -> m' [(SomeDimensionIx, Description, Message)]

  step z RasterInput{rLoad,rDescription,rDimension} = do
    r <- hoist (rLoad ix)
    return $ case r of
      Right _ -> z
      Left e  -> (SomeDimensionIx rDimension ix, rDescription, show e) : z
    
  step z PointInput{pLoad,pDescription,pDimension} = do
    r <- hoist (pLoad ix)
    return $ case r of
      Right _ -> z
      Left e  -> (SomeDimensionIx pDimension ix, pDescription, show e) : z

  step z AreaInput{aLoad,aDescription,aDimension} = do
    r <- hoist (aLoad ix)
    return $ case r of
      Right _ -> z
      Left e  -> (SomeDimensionIx aDimension ix, aDescription, show e) : z

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

  step z (AdaptDim _ (WithFingerprint _ f) v) =
    let loop z' (x:xs) = do
          z'' <- hoist (getMissingInputs v x)
          if null z'' then return z' else loop z'' xs
        loop z' [] = return z'
    in loop z (toList (f (dimension v) ix))
  step z (CheckPoint _ v) = step z v
  step z (Describe   _ v) = step z v

  step z (Map _ v) = step z v

prettyAST
  :: forall m exp t crs dim a. Typeable (Variable m exp t crs dim a)
  => Variable m exp t crs dim a -> Doc
prettyAST = goV where
  go, goV :: Variable m exp t crs dim a -> Doc
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
  go (DimensionDependant (WithFingerprint _ _) dim) =
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

