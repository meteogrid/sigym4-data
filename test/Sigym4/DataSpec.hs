{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Sigym4.DataSpec ( spec, main ) where

import           Sigym4.Data hiding (describe, map)
import qualified Sigym4.Data as D
import qualified Sigym4.Data.AST as AST
import           Sigym4.Data.Units

import           Control.Newtype
import           Data.Functor.Identity
import           Data.List (isInfixOf)
import           Data.Maybe

import           Test.Hspec

main :: IO ()
main = hspec spec

newtype Temperature = Temperature Double
  deriving
    ( Eq, Ord, Show, Num
    , RealFrac, Real, Fractional, Floating, RealFloat, IEEE, Storable)

instance HasUnits Temperature where
  type Units Temperature = ()
  type MachineType Temperature = Double

spec :: Spec
spec = do
  describe "prettyAST" $ do
    let v :: DummyRasterVar (Epsg 23030) Observation Temperature
        v = dummyRasterInput "dummy"
              (const (Left NotAvailable))
              (const (return (Left NotAvailable)))
              (Schedule [cron|0 0 * * *|])

    it "shows description" $ do
      show (prettyAST v) `shouldSatisfy` isInfixOf "dummy"

    it "shows dimension" $ do
      show (prettyAST v) `shouldSatisfy` isInfixOf "0 0 * * *"

    it "shows crs" $ do
      show (prettyAST v) `shouldSatisfy` isInfixOf "23030"

    it "shows type" $ do
      show (prettyAST v) `shouldSatisfy` isInfixOf "Temperature"

    it "shows dimension type" $ do
      show (prettyAST v) `shouldSatisfy` isInfixOf "Observation"

    it "can describe" $ do
      let v' = D.describe "una variable" v
      show (prettyAST v') `shouldSatisfy`
        isInfixOf "una variable"
      show (prettyAST v) `shouldSatisfy` isInfixOf "dummy"


  describe "adaptDim" $ do
    let tObs :: DummyRasterVar (Epsg 23030) Observation Temperature
        tObs = dummyRasterInput "temperatura observada"
              (const (Left NotAvailable))
              (const (return (Left NotAvailable)))
              (Schedule [cron|0 * * * *|])

        tPred :: DummyRasterVar (Epsg 23030) Prediction Temperature
        tPred = dummyRasterInput "temperatura inventada"
              (const (Left NotAvailable))
              (const (return (Left NotAvailable)))
              ([0,60..24*60] :* Schedule [cron|0 0 * * *|])

        predictedTime (horizon:*runTime) = addHorizon horizon runTime

        closestObservedTime dObs ixPred =
          let bestTime = idfloor dObs (coerceTime (predictedTime ixPred))
          in maybeToList (fmap unQ bestTime)

        adaptedObs = adaptDim (dimension tPred) closestObservedTime tObs

        err :: DummyRasterVar (Epsg 23030) Prediction Temperature
        err = D.describe "predErr" $
          D.zipWith (D.fp "v1" (\o p -> o*o - p*p)) adaptedObs tPred

    it "can be pretty printed" $ do
      show (prettyAST err) `shouldSatisfy` isInfixOf "predErr"
      show (prettyAST err) `shouldSatisfy` isInfixOf "AdaptDim"
      show (prettyAST err) `shouldSatisfy` isInfixOf "ZipWith"

    it "can calculate missing inputs" $ do
      let ix = Hour 6 :* datetime 2016 11 28 0 0
          missing = runDummy (getMissingInputs err ix)
      length missing `shouldBe` 2
      map missingIx missing `shouldMatchList` [
          SomeDimensionIx (dimension tObs)  (datetime 2016 11 28 6 0)
        , SomeDimensionIx (dimension tPred) ix
        ]




datetime :: Newtype t UTCTime
         => Integer -> Int -> Int -> Int -> Int -> t
datetime y m d h mn
  = pack (UTCTime (fromGregorian y m d) (fromIntegral (h*60+mn)*60))



newtype DummyInterpreter a = DummyInterpreter (Identity a)
  deriving (Functor, Applicative, Monad)

runDummy :: DummyInterpreter a -> a
runDummy (DummyInterpreter a) = runIdentity a

type DummyVar = Variable DummyInterpreter 
type DummyRasterVar = Variable DummyInterpreter RasterT

dummyRasterInput
  :: AST.IsRasterInput DummyInterpreter crs dim a
  => Description
  -> (DimensionIx dim -> Either LoadError (DummyBand crs a))
  -> (DimensionIx dim -> DummyInterpreter (Either LoadError Fingerprint))
  -> dim -> DummyRasterVar crs dim a
dummyRasterInput desc res f d = AST.RasterInput
  { AST.rLoad        = return . res
  , AST.rFingerprint = f
  , AST.rDimension   = d
  , AST.rDescription = desc
  }

data DummyBand crs a = DummyBand
  { dummyDescription :: Description
  }
type instance AST.Exp DummyInterpreter = Identity
type instance AST.RasterBand DummyInterpreter crs a = DummyBand crs a

instance Storable a => AST.HasReadBlock (DummyBand crs a) DummyInterpreter a
  where readBlock = error "not implemented"
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

instance AST.HasDescription (DummyBand crs a) DummyInterpreter where
  description = return . dummyDescription
