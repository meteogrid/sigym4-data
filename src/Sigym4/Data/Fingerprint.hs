{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Sigym4.Data.Fingerprint (
  Fingerprint
, WithFingerprint (..)
, HasFingerprint (..)
, fp
, sinkFingerPrint
) where

import           Control.DeepSeq (NFData(rnf))
import           Crypto.Hash
import           Crypto.Hash.Conduit (sinkHash)
import           Data.Conduit
import qualified Data.ByteString.Char8 as BS
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

newtype Fingerprint = FP { unFP :: Digest SHA1 }
  deriving (Eq, Ord, Show, NFData)

data WithFingerprint a = WithFingerprint Fingerprint a
  deriving (Eq, Ord, Show)

instance NFData a => NFData (WithFingerprint a) where
  rnf (WithFingerprint f a) = rnf f `seq` rnf a

class HasFingerprint o where
  fingerprint :: o -> Fingerprint


instance HasFingerprint (WithFingerprint a) where
  fingerprint (WithFingerprint f _) = f

instance HasFingerprint Fingerprint where
  fingerprint = id

instance HasFingerprint BS.ByteString where
  fingerprint = FP . hash

instance Monoid Fingerprint where
  mempty = FP (hashFinalize hashInit)
  mappend (FP a) (FP b) = FP . hashFinalize $ hashUpdates hashInit [a, b]
  mconcat = FP . hashFinalize . hashUpdates hashInit . map unFP

fp ::QuasiQuoter
fp = QuasiQuoter
  { quoteExp = \s -> do
      s2 <- ((s++) . loc_filename) <$> location
      [e|WithFingerprint (FP (hash (BS.pack s2)))|]
  , quotePat  = undefined
  , quoteType = const (fail "Cannot apply cron quasiquoter in types")
  , quoteDec  = const (fail "Cannot apply cron quasiquoter in declarations")
  }
sinkFingerPrint :: Monad m => Consumer BS.ByteString m Fingerprint
sinkFingerPrint = fmap FP sinkHash
