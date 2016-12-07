{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sigym4.Data.MaybeSpec ( spec, main ) where

import           Sigym4.Data.Maybe
import           Sigym4.Data.Null
import           Prelude hiding (Maybe(..), maybe)

import           Foreign.Storable
import           Test.Hspec

main :: IO ()
main = hspec spec




spec :: Spec
spec = do
  describe "Behaves as Num" $ do
    it "valid values produce valid value" $
      10 + 10 `shouldBe` (20 :: Maybe TestValue)
    it "any nullValue produces Nothing" $
      (-42) + 10 `shouldBe` (Nothing :: Maybe TestValue)

newtype TestValue = TV Double
  deriving
    ( Eq, Ord, Show, Num
    , RealFrac, Real, Fractional, Floating, RealFloat, Storable)

instance HasNull TestValue where
  isNull    = (==nullValue)
  nullValue = TV (-42)
