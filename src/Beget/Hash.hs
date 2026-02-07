{-# LANGUAGE UndecidableInstances #-}

module Beget.Hash
  ( BegetHash (..)
  , BegetHashable (..)
  )
where

import ChibiHash (chibihash64)
import Codec.Serialise (Serialise, serialise)

newtype BegetHash = BegetHash Word64
  deriving stock (Show)
  deriving newtype (Eq, Serialise)

class BegetHashable a where
  begetHash :: a -> BegetHash

instance {-# OVERLAPPABLE #-} Serialise a => BegetHashable a where
  begetHash :: a -> BegetHash
  begetHash x = BegetHash (chibihash64 (toStrict (serialise x)) 0)
