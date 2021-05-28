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
  ( -- * Types related to Curve25519
    Elem(..), Scalar(..)
  ) where

import Foreign.Ptr            ( castPtr, Ptr )
import Foreign.Storable       ( Storable(..) )
import Raaz.Core.Prelude
import Raaz.Core.Encode
import Raaz.Core.Types.Endian
import Raaz.Core.Types.Equality
import Raaz.Core.Types.Tuple

-- | The Word used for elements, scalars in this module.


-- | Element in the Galois field 𝔽ₚ where p is the prime 2²⁵⁵ - 19.
newtype Elem = Elem { unElem :: WORD }
             deriving (Equality, Eq)

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

type WORD = Tuple 4 (LE Word64)

-- Cast to the appr
castToWORDPtr :: Ptr a -> Ptr WORD
castToWORDPtr = castPtr

undefinedWORD :: WORD
undefinedWORD = undefined
