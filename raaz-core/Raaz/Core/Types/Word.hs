{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveDataTypeable         #-}


-- | This module also provide explicitly endianness encoded versions
-- of Word32 and Word64 which are instances of `EndianStore`. These
-- types inherit their parent type's `Num` instance (besides `Ord`,
-- `Eq` etc). The advantage is the following uniformity in their usage
-- in Haskell code:
--
--   1. Numeric constants are represented in their Haskell notation
--      (which is big endian). For example 0xF0 represents the number
--      240 whether it is @`LE` Word32@ or @`BE` Word32@ or just `Word32`.
--
--   2. The normal arithmetic work on them.
--
--   3. They have the same printed form except for the constructor
--      sticking around.
--
-- Therefore, as far as Haskell programmers are concerned, @`LE`
-- Word32@ and @`BE` Word32@ should be treated as `Word32` for all
-- algorithmic aspects. Similarly, @`LE` Word64@ and @`BE` Word64@
-- should be treated as `Word64`.
--
-- When defining other endian sensitive data types like hashes, we
-- expect users to use these endian safe types. For example SHA1 can
-- be defined as
--
-- > data SHA1 = SHA1 (BE Word32) (BE Word32) (BE Word32) (BE Word32) (BE Word32)
--
-- Then the `EndianStore` instance boils down to storing the words in
-- correct order.

module Raaz.Core.Types.Word
       ( LE, BE
       ) where

import Data.Bits
import Data.Typeable
import Data.Word
import Foreign.Storable
import Test.QuickCheck          (Arbitrary)
import Raaz.Core.Classes


{-

Developers notes:
-----------------

Make sure that the endian encoded version does not have any
performance penalty. We may have to stare at the core code generated
by ghc.

-}

-- | Little endian version of the word type @w@
newtype LE w = LE w
    deriving ( Arbitrary, Bounded, Enum, Read, Show
             , Integral, Num, Real, Eq, EqWord, Ord
             , Bits, Storable, Typeable
             )


-- | Big endian version of the word type @w@
newtype BE w = BE w
    deriving ( Arbitrary, Bounded, Enum, Read, Show
             , Integral, Num, Real, Eq, EqWord, Ord
             , Bits, Storable, Typeable
             )

{-|

Developers notes:

The next set of conversion functions are intensionally not exported
and are defined only to aid readability of the Storable instance
declaration. At first glance it might appear that they could be useful
but their export can cause confusion.

-}


instance HasName w => HasName (LE w) where
  getName (LE w) = "LE " ++ getName w

instance HasName w => HasName (BE w) where
  getName (BE w) = "BE " ++ getName w

instance CryptoCoerce w (LE w) where
  cryptoCoerce = LE

instance CryptoCoerce w (BE w) where
  cryptoCoerce = BE
