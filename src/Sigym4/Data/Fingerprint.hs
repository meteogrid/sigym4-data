{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Sigym4.Data.Fingerprint (
  Fingerprint
, WithFingerprint (..)
, HasFingerprint (..)
, fp
) where

import           Control.DeepSeq (NFData)
import           Crypto.Hash
import           Data.ByteString (ByteString)
import           Language.Haskell.TH

newtype Fingerprint = FP { unFP :: Digest SHA1 }
  deriving (Eq, Ord, Show, NFData)

data WithFingerprint a = WithFingerprint Fingerprint a
  deriving (Eq, Ord, Show)

class HasFingerprint o where
  fingerprint :: o -> Fingerprint


instance HasFingerprint (WithFingerprint a) where
  fingerprint (WithFingerprint f _) = f

instance HasFingerprint Fingerprint where
  fingerprint = id

fp :: ByteString -> a -> WithFingerprint a
fp s = WithFingerprint (FP (hash s))

instance Monoid Fingerprint where
  mempty = FP (hashFinalize hashInit)
  mappend (FP a) (FP b) = FP . hashFinalize $ hashUpdates hashInit [a, b]
  mconcat = FP . hashFinalize . hashUpdates hashInit . map unFP


mkFingerprint :: Name -> Q Exp
mkFingerprint = error "TBD"
