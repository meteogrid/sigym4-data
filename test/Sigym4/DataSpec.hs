{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Sigym4.DataSpec ( spec, main ) where

import           Sigym4.Data hiding (describe)
import qualified Sigym4.Data
import qualified Sigym4.Data.AST as AST
import           Sigym4.Data.Units

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
    let v :: DummyRasterVar (Epsg 23030) CronSchedule Temperature
        v = dummyRasterInput "dummy"
              (const (Left NotAvailable))
              (const (return (Left NotAvailable)))
              [cron|0 0 * * *|]

    it "shows description" $ do
      show (prettyAST v) `shouldSatisfy` isInfixOf "dummy"

    it "can describe" $ do
      let v' = Sigym4.Data.describe "una variable" v
      show (prettyAST v') `shouldSatisfy`
        isInfixOf "una variable"
      show (prettyAST v) `shouldSatisfy` isInfixOf "dummy"

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
