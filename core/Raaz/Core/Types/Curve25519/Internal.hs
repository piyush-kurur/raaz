{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Raaz.Core.Types.Curve25519.Internal
       ( prime
       ) where

import           Foreign.Storable            ( Storable(..) )

import Raaz.Core.Prelude
import Raaz.Core.Types.Equality
import Raaz.Core.Types.Endian
import Raaz.Core.Types.Tuple

prime :: Integer
prime = 2^(255 :: Int) - 19

-- | Elements of the field GF(2^255 - 19).
newtype GF25519 = GF (Tuple 4 (LE Word64)) deriving (Equality, Eq, Show, Storable, EndianStore)
