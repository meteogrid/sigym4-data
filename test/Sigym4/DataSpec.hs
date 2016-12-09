-- Para los tests de cosas que no deben compilar
{-# OPTIONS_GHC -fdefer-type-errors #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Sigym4.DataSpec ( spec, main ) where

import           Sigym4.Data hiding (describe, map, const)
import qualified Sigym4.Data as D
import qualified Sigym4.Data.AST as AST
import           Sigym4.Data.Units
import           Sigym4.Data.Null

import           Control.Monad.Trans.Either
import           Control.Newtype
import           Data.Functor.Identity
import           Data.List (isInfixOf)
import qualified Data.Vector.Storable as St
import           Data.Maybe

import           Test.Hspec
import           Test.ShouldNotTypecheck (shouldNotTypecheck)

main :: IO ()
main = hspec spec

newtype Temperature = Temperature Double
  deriving
    ( Eq, Ord, Show, Num
    , RealFrac, Real, Fractional, Floating, RealFloat, Storable)

instance HasNull Temperature where nullValue = (-9999)
  


instance HasUnits Temperature Identity where
  type Units Temperature = ()
  type MachineType Temperature = Double

spec :: Spec
spec = do
  describe "prettyAST" $ do
    let v :: DummyRasterVar (Epsg 23030) Observation Temperature
        v = dummyRasterInput "dummy"
              (const (throwError NotAvailable))
              (const (throwError NotAvailable))
              (Schedule [cron|0 0 * * *|])

    it "shows description" $ do
      show (prettyAST v) `shouldSatisfy` isInfixOf "dummy"

    it "shows dimension" $ do
      show (prettyAST v) `shouldSatisfy` isInfixOf "0 0 * * *"

    it "handles cycles" $ do
      let v' = D.zipWith ([fp|v1|] (+)) v v'
      show (prettyAST v') `shouldSatisfy` isInfixOf "..."

    describe "only shows type of \"describe\" nodes" $ do
      let v' = D.describe "una variable" v

      it "shows description" $ do
        show (prettyAST v') `shouldSatisfy` isInfixOf "una variable"

      it "shows crs" $ do
        show v' `shouldSatisfy` isInfixOf "23030"

      it "shows type" $ do
        show v' `shouldSatisfy` isInfixOf "Temperature"

      it "shows dimension type" $ do
        show v' `shouldSatisfy` isInfixOf "Observation"



  describe "adaptDim" $ do
    let tObs :: DummyRasterVar (Epsg 23030) Observation Temperature
        tObs = dummyRasterInput "temperatura observada"
              (const (throwError NotAvailable))
              (const (throwError NotAvailable))
              (Schedule [cron|0 * * * *|])

        tPred :: DummyRasterVar (Epsg 23030) Prediction Temperature
        tPred = dummyRasterInput "temperatura inventada"
              (const (throwError NotAvailable))
              (const (throwError NotAvailable))
              ([0,60..24*60] :* Schedule [cron|0 0 * * *|])

        sqErr = [fp|version1|] $ \o p -> o*o - p*p

        predictedTime (horizon:*runTime) = addHorizon horizon runTime

        closestObservedTime dObs ixPred =
          let bestTime = idfloor dObs (coerceTime (predictedTime ixPred))
          in maybeToList (fmap unQ bestTime)

        adaptedObs = adaptDim (dimension tPred) (closestObservedTime (dimension tObs)) tObs

        tErr :: DummyRasterVar (Epsg 23030) Prediction Temperature
        tErr = D.describe "predErr" $
                 D.zipWith sqErr adaptedObs tPred

    it "should not typecheck without adaptDim" $ shouldNotTypecheck $
      let tErrBad :: DummyRasterVar (Epsg 23030) Prediction Temperature
          tErrBad = D.describe "predErr" $ D.zipWith sqErr tObs tPred
      in tErrBad

    it "can be pretty printed" $ do
      show (prettyAST tErr) `shouldSatisfy` isInfixOf "predErr"
      show (prettyAST tErr) `shouldSatisfy` isInfixOf "AdaptDim"
      show (prettyAST tErr) `shouldSatisfy` isInfixOf "ZipWith"

    describe "getMissingInputs" $ do
      it "can calculate" $ do
        let ix = Hour 6 :* datetime 2016 11 28 0 0
            Right missing = runDummy (getMissingInputs tErr ix)
        length missing `shouldBe` 2
        map missingIx missing `shouldMatchList` [
            SomeDimensionIx (dimension tObs)  (datetime 2016 11 28 6 0)
          , SomeDimensionIx (dimension tPred) ix
          ]

      it "handles cycles" $ do
        let ix = Hour 6 :* datetime 2016 11 28 0 0
            Right missing = runDummy (getMissingInputs v' ix)
            v' = D.zipWith ([fp|v1|] (+)) tErr v'
        length missing `shouldSatisfy` (>0)


    it "marks failed adaptation as missing input" $ do
      let tPredGood = dummyRasterInput "temperatura inventada"
                (const (return undefined))
                (const (throwError NotAvailable))
                ([0,60..24*60] :* Schedule [cron|0 0 * * *|])
          tObsBad = adaptDim (dimension tPred) badAdaptor tObs
            where badAdaptor = const []
          tPredBad = D.describe "predErr" $
                      D.zipWith sqErr tObsBad tPredGood
          ix = Hour 6 :* datetime 2016 11 28 0 0
          Right missing = runDummy (getMissingInputs tPredBad ix)
      map missingIx missing `shouldMatchList` [
          SomeDimensionIx (dimension tPred) ix
        ]


datetime :: Newtype t UTCTime
         => Integer -> Int -> Int -> Int -> Int -> t
datetime y m d h mn
  = pack (UTCTime (fromGregorian y m d) (fromIntegral (h*60+mn)*60))



newtype DummyInterpreter a = DummyInterpreter (EitherT LoadError Identity a)
  deriving (Functor, Applicative, Monad, MonadError LoadError)

runDummy :: DummyInterpreter a -> Either LoadError a
runDummy (DummyInterpreter a) = runIdentity (runEitherT a)

type DummyVar = Variable DummyInterpreter 
type DummyRasterVar = Variable DummyInterpreter RasterT

dummyRasterInput
  :: IsInput DummyInterpreter RasterT crs dim a
  => Description
  -> (DimensionIx dim -> DummyInterpreter (DummyBand crs a))
  -> (DimensionIx dim -> DummyInterpreter Fingerprint)
  -> dim -> DummyRasterVar crs dim a
dummyRasterInput desc res f d = input $ DummyRasterInput
  { rLoad        = res
  , rFingerprint = f
  , rDimension   = d
  , rDescription = desc
  }


data instance AST.Loader DummyInterpreter RasterT crs dim a = DummyRasterInput
  { rLoad :: DimensionIx dim -> DummyInterpreter (DummyBand crs a)
  , rFingerprint :: DimensionIx dim -> DummyInterpreter Fingerprint
  , rDimension   :: dim
  , rDescription :: Description
  }

type DummyRasterInput = AST.Loader DummyInterpreter RasterT

instance IsVariable DummyInterpreter RasterT crs dim a
  => AST.HasLoad DummyInterpreter RasterT crs dim a where
  type Dataset DummyInterpreter RasterT crs a = DummyBand crs a
  load = rLoad

instance AST.HasCalculateFingerprint DummyInterpreter dim (DummyRasterInput crs dim a) where
  calculateFingerprint = rFingerprint

instance HasDimension (DummyRasterInput crs dim a) dim where dimension = rDimension

instance AST.HasDescription (DummyRasterInput crs dim a) where
  description = rDescription

data DummyBand crs a = DummyBand
  { dummyDescription :: Description
  }
type instance AST.Exp DummyInterpreter = Identity
type instance AST.RasterBand DummyInterpreter crs a = DummyBand crs a


instance Storable a => AST.HasReadBlock (DummyBand crs a) DummyInterpreter a where
  type BlockVectorType (DummyBand crs a) DummyInterpreter = St.Vector
  readBlock = error "not implemented"

instance AST.HasBlockSize (DummyBand crs a) DummyInterpreter where
  blockSize = return . const (Size 256)
instance AST.HasRasterSize (DummyBand crs a) DummyInterpreter where
  rasterSize = return . const (Size 2000)
instance AST.HasNodataValue (DummyBand crs a) DummyInterpreter a where
  nodataValue = return . const Nothing
instance AST.HasGeoReference (DummyBand crs a) DummyInterpreter crs where
  geoReference = return . const gr where
    gr = either (error "unreachable") id
       $ mkGeoReference (Extent 0 100) (Size 10)
instance AST.HasCrs (DummyBand crs a) DummyInterpreter where
  getCrs = return . const (fromMaybe (error "unreachable") (epsgCrs 4326))

instance AST.HasDescription (DummyBand crs a) where
  description = dummyDescription
