{-# LANGUAGE UndecidableInstances #-}

module Beget.Hash
  ( BegetHash (..)
  , BegetHashable (..)
  )
where

import ChibiHash (chibihash64)
import Codec.Serialise (Serialise, serialise)
import Data.ByteString qualified as ByteString
import Data.Word (Word64)

newtype BegetHash = BegetHash Word64
  deriving stock (Show)
  deriving newtype (Eq, Serialise)

class BegetHashable a where
  begetHash :: a -> BegetHash

instance {-# OVERLAPPABLE #-} Serialise a => BegetHashable a where
  begetHash :: a -> BegetHash
  begetHash x = BegetHash (chibihash64 (ByteString.toStrict (serialise x)) 0)
