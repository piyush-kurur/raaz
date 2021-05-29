{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
--
-- Module      : Raaz.Core.Types.Curve25519
-- Copyright   : (c) Piyush P Kurur, 2019
-- License     : Apache-2.0 OR BSD-3-Clause
-- Maintainer  : Piyush P Kurur <ppk@iitpkd.ac.in>
-- Stability   : experimental
--

module Raaz.Core.Types.Curve25519
  ( -- * Curve25519 Types
    Elem(..), Scalar(..)
    -- ** Parameters
  , genX
  , prime, order
    -- ** Unsafe functions
  , unsafeFromElem, unsafeToElem
  , unsafeFromScalar, unsafeToScalar
  ) where

import           Foreign.Ptr            ( castPtr, Ptr )
import           Foreign.Storable       ( Storable(..) )
import qualified Data.Vector.Unboxed as V
import           Raaz.Core.Prelude
import           Raaz.Core.Encode
import           Raaz.Core.Types.Endian
import           Raaz.Core.Types.Equality
import           Raaz.Core.Types.Tuple hiding (map)

-- | The Word used for elements, scalars in this module.


-- | Element in the Galois field 𝔽ₚ where p is the prime 2²⁵⁵ - 19.
newtype Elem = Elem { unElem :: WORD }
             deriving (Equality, Eq)

-- | The (x-cordinate of the) generator.
genX :: Elem
genX = Elem $ unsafeFromList [9,0,0,0]

-- | Convert field element to the corresponding integer. The following
-- inequality should be true for all elements
--
-- >
-- > 0 ≤ unsafeFromElem elem< 2²⁵⁵ - 19.
-- >
unsafeFromElem :: Elem -> Integer
unsafeFromElem =  w2I . unElem

-- | Convert the integer to the corresponding field element
--
unsafeToElem :: Integer -> Elem
unsafeToElem = Elem . i2W . modPrime
  where modPrime x = x `mod` prime

instance Show Elem where
  show = showBase16

instance IsString Elem where
  fromString = fromBase16

instance Storable Elem where
  sizeOf _    = sizeOf undefinedWORD
  alignment _ = alignment undefinedWORD

  peek     = fmap Elem . peek . castPtr
  poke ptr =  poke (castToWORDPtr ptr) . unElem

instance EndianStore Elem where
  load         = fmap Elem . load . castPtr
  store ptr    = store (castToWORDPtr ptr) . unElem
  adjustEndian = adjustEndian . castToWORDPtr

instance Encodable Elem

-- | Elliptic curve protocols often involve computing the point nQ for
-- some base point Q. The n is an integer which we call the scalar
-- associated with the curve.
newtype Scalar = Scalar { unScalar :: WORD }
               deriving (Equality, Eq)

-- | Convert from scalar to the associated integer.
unsafeFromScalar :: Scalar -> Integer
unsafeFromScalar =  w2I . unScalar

-- | Convert the integer to the corresponding field element
unsafeToScalar :: Integer -> Scalar
unsafeToScalar = Scalar . i2W . clamp
  where clamp = foldr1 (.) $ cBits ++ sBits
        sBits = [flip setBit 254]
        cBits = map (flip clearBit) [0,1,2, 255]

instance Show Scalar where
  show = showBase16

instance IsString Scalar where
  fromString = fromBase16

instance Storable Scalar where
  sizeOf _    = sizeOf undefinedWORD
  alignment _ = alignment undefinedWORD

  peek     = fmap Scalar . peek . castPtr
  poke ptr =  poke (castToWORDPtr ptr) . unScalar

instance EndianStore Scalar where
  load         = fmap Scalar . load . castPtr
  store ptr    = store (castToWORDPtr ptr) . unScalar
  adjustEndian = adjustEndian . castToWORDPtr

instance Encodable Scalar

type Limb = LE Word64
type WORD = Tuple 4 Limb

-- | The prime number p = 2²⁵⁵ - 19 associated with the field Galois
-- field 𝔽ₚ on which the curve is defined.
prime :: Integer
prime = twoPow255 - 19
  where twoPow255 = setBit 0 255

-- | The order of the group
order :: Integer
order  = twoPow252 + 0x14def9dea2f79cd65812631a5cf5d3ed
  where twoPow252 = setBit 0 252

------------- Low level helper functions not to be exported ---------------

w2I :: WORD -> Integer
w2I = V.foldr fld 0 . unsafeToVector
  where fld leW64 ival = ival `shiftL` 64 + toInteger leW64

i2W :: Integer -> WORD
i2W ival = unsafeFromList [a0, a1, a2, a3]
  where a0    = fromInteger ival
        a1    = fromInteger (ival `shiftR` 64)
        a2    = fromInteger (ival `shiftR` 128)
        a3    = fromInteger (ival `shiftR` 192)

castToWORDPtr :: Ptr a -> Ptr WORD
castToWORDPtr = castPtr

undefinedWORD :: WORD
undefinedWORD = undefined
