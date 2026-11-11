module Beget.Value
  ( Value
  )
where

import Beget.Hash (BegetHashable)
import Codec.Serialise (Serialise)
import Data.Hashable (Hashable)
import Type.Reflection (Typeable)

class (Typeable a, Show a, Serialise a, BegetHashable a, Hashable a) => Value a
