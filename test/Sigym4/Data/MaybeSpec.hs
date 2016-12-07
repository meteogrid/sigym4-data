{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sigym4.Data.MaybeSpec ( spec, main ) where

import           Sigym4.Data.Maybe
import           Sigym4.Data.Null
import           Prelude hiding (Maybe(..), maybe)

import           Control.Newtype
import           Data.Coerce (coerce)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as St
import           Data.Vector.Unboxed.Deriving (derivingUnbox)
import           Foreign.Storable
import           Test.QuickCheck
import           Test.Hspec
import           Test.Hspec.QuickCheck

main :: IO ()
main = hspec spec




spec :: Spec
spec = do
  describe "Maybe" $ do
    describe "Behaves as Num" $ do
      it "valid values produce valid value" $
        10 + 10 `shouldBe` (20 :: Maybe TestValue)
      it "any nullValue produces Nothing" $
        Nothing + 10 `shouldBe` (Nothing  :: Maybe TestValue)

    describe "toMaskAndVector and fromMaskAndVector" $ do
      prop "from . to should be id" $
        \(vec :: U.Vector (Maybe TestValue)) ->
            let vec' = uncurry fromMaskAndVector . toMaskAndVector $ vec
            in counterexample (show vec') $ vec == vec'

      prop "to . from should be id" $ forAll maskAndVector $ \(mask,vec) ->
            let (mask', vec') = toMaskAndVector $ fromMaskAndVector mask vec
            in counterexample (show (mask', vec')) $ vec == vec' && mask == mask'

    prop "map over storable vector does not touch null values" $ do
     \(vec :: St.Vector (Maybe TestValue)) ->
      let vec' = St.map (+10) vec
          sVec = St.sum ((St.map (fromMaybe 0)) vec)
          sVec' = St.sum ((St.map (fromMaybe 0)) vec')
          nJusts = St.length (St.filter isJust vec)
      in counterexample (show (sVec, sVec', nJusts)) $
          St.map isNothing vec == St.map isNothing vec' &&
          sVec' == sVec + fromIntegral (nJusts * 10)

  describe "Nullable" $ do
    describe "Behaves as Num" $ do
      it "valid values produce valid value" $
        10 + 10 `shouldBe` (20 :: Nullable TestValue)
      it "any nullValue produces Nothing" $
        Null Nothing + 10 `shouldBe` (Null Nothing  :: Nullable TestValue)

      it "unfortunately, allows producing Just nullValue" $
        (-52) + 10 `shouldBe` (pure nullValue  :: Nullable TestValue)

    describe "toNullableVectorWith and nullableFromVectorWith" $ do
      prop "from . to should be id regardless nodata value outside of domain" $
        \( (TV . negate . getPositive) -> nd
          , vec :: U.Vector (Nullable TestValue)
          ) ->
            let vec' = nullableFromVectorWith nd . toNullableVectorWith nd $ vec
            in counterexample (show vec') $ vec == vec'

    prop "map over storable vector does not touch null values" $ do
     \(vec :: St.Vector (Nullable TestValue)) ->
      let vec' = St.map (+10) vec
          sVec = St.sum ((St.map (fromMaybe 0)) vec)
          sVec' = St.sum ((St.map (fromMaybe 0)) vec')
          nJusts = St.length (St.filter isJust vec)
      in counterexample (show (sVec, sVec', nJusts)) $
          St.map isNothing vec == St.map isNothing vec' &&
          sVec' == sVec + fromIntegral (nJusts * 10)

maskAndVector :: Gen (U.Vector MaskType, U.Vector TestValue)
maskAndVector = do
  n <- getPositive <$> arbitrary
  (,) <$> (U.fromList <$> vectorOf n arbitrary) <*> (U.fromList <$> vectorOf n arbitrary)


newtype TestValue = TV Double
  deriving
    ( Ord, Show, Num
    , RealFrac, Real, Fractional, Floating, RealFloat, Storable
    , Arbitrary)

instance Eq TestValue where
  TV a == TV b = abs (a-b) < 1e-12

derivingUnbox "TestValue"
    [t| TestValue -> Double |]
    [| coerce |]
    [| coerce |]


instance HasNull TestValue where
  isNull    = (==nullValue)
  nullValue = TV (-42)

instance Arbitrary (Nullable TestValue) where
  arbitrary = pack <$> arbitrary

instance Arbitrary (St.Vector (Maybe TestValue)) where
  arbitrary = St.fromList <$> arbitrary

instance Arbitrary (St.Vector (Nullable TestValue)) where
  arbitrary = St.fromList <$> arbitrary

instance Arbitrary (U.Vector (Nullable TestValue)) where
  arbitrary = U.fromList <$> arbitrary

instance Arbitrary (U.Vector MaskType) where
  arbitrary = U.fromList <$> listOf (oneof [pure 1, pure 0])


instance Arbitrary (Maybe TestValue) where
  arbitrary = oneof [ pure Nothing, pure <$> arbitrary ]

instance Arbitrary (U.Vector TestValue) where
  arbitrary = U.fromList <$> arbitrary

instance Arbitrary (U.Vector (Maybe TestValue)) where
  arbitrary = U.fromList <$> arbitrary
