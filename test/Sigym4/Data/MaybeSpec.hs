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
import           Data.Vector.Unboxed.Deriving (derivingUnbox)
import           Foreign.Storable
import           Test.QuickCheck
import           Test.Hspec
import           Test.Hspec.QuickCheck

main :: IO ()
main = hspec spec




spec :: Spec
spec = do
  describe "Nullable" $ do
    describe "Behaves as Num" $ do
      it "valid values produce valid value" $
        10 + 10 `shouldBe` (20 :: Nullable TestValue)
      it "any nullValue produces Nothing" $
        Nullable Nothing + 10 `shouldBe` (Nullable Nothing  :: Nullable TestValue)

    describe "toNullableVectorWith and nullableFromVectorWith" $ do
      prop "from . to should be biyective regardless nodata value outside of domain" $
        \( (TV . negate . getPositive) -> nd
          , vec :: U.Vector (Nullable TestValue)
          ) ->
            let vec' = nullableFromVectorWith nd . toNullableVectorWith nd $ vec
            in counterexample (show vec') $ vec == vec'

newtype TestValue = TV Double
  deriving
    ( Eq, Ord, Show, Num
    , RealFrac, Real, Fractional, Floating, RealFloat, Storable
    , Arbitrary)
derivingUnbox "TestValue"
    [t| TestValue -> Double |]
    [| coerce |]
    [| coerce |]

instance HasNull TestValue where
  isNull    = (==nullValue)
  nullValue = TV (-42)

instance Arbitrary (Nullable TestValue) where
  arbitrary = pack <$> arbitrary

instance Arbitrary (U.Vector (Nullable TestValue)) where
  arbitrary = U.fromList <$> arbitrary


instance Arbitrary (Maybe TestValue) where
  arbitrary = oneof [ pure Nothing, fromNullable . getPositive <$> arbitrary ]
{-
instance Arbitrary (U.Vector (Maybe TestValue)) where
  arbitrary = U.fromList <$> arbitrary
-}
