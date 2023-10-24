{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Some common instances that are required by the test cases.
module Tests.Core.Instances () where

import GHC.TypeNats
import Data.Vector.Unboxed as VU
import Tests.Core.Imports

import Raaz.Primitive.Poly1305.Internal as Poly1305
import Raaz.KeyExchange.X25519.Internal as X25519

import Raaz.Core.Types.Internal
import Raaz.Primitive.Keyed.Internal as Keyed


instance Arbitrary w => Arbitrary (LE w) where
  arbitrary = littleEndian <$> arbitrary

instance Arbitrary w => Arbitrary (BE w) where
  arbitrary = bigEndian <$> arbitrary

instance Arbitrary w => Arbitrary (BYTES w) where
  arbitrary = BYTES <$> arbitrary

instance Arbitrary ByteString where
  arbitrary = pack <$> arbitrary

instance (VU.Unbox a, KnownNat n, Arbitrary a) => Arbitrary (Tuple n a) where
  arbitrary = gen
    where gen = unsafeFromList <$> vector dim
          dim = dimension $ getUndef gen
          getUndef :: Gen (Tuple n a) -> Tuple n a
          getUndef = undefined

---------------   Arbitrary instances for Hashes ----------------

instance Arbitrary Sha256 where
  arbitrary = genEncodable

instance Arbitrary Sha512 where
  arbitrary = genEncodable

instance Arbitrary Blake2b where
  arbitrary = genEncodable

instance Arbitrary Blake2s where
  arbitrary = genEncodable

---------------- Parameter block for Blake          -------------

---------------- Arbitrary instaces of encoded data --------------

instance Arbitrary Base16 where
  arbitrary =  encodeByteString . pack <$> listOf arbitrary

instance Arbitrary Base64 where
  arbitrary =  encodeByteString . pack <$> listOf arbitrary

------------------ For ChaCha20 types -------------------------

instance Arbitrary (Key ChaCha20) where
  arbitrary = genEncodable

instance Arbitrary (Nounce ChaCha20) where
  arbitrary = genEncodable

instance Arbitrary (BlockCount ChaCha20) where
  arbitrary = toEnum <$> arbitrary

------------------ For XChaCha20 types -------------------------

instance Arbitrary (Key XChaCha20) where
  arbitrary = genEncodable

instance Arbitrary (Key (Keyed prim)) where
  arbitrary = Keyed.Key . pack <$> listOf1 arbitrary

instance Arbitrary (Nounce XChaCha20) where
  arbitrary = genEncodable

instance Arbitrary (BlockCount XChaCha20) where
  arbitrary = toEnum <$> arbitrary

------------------ Arbitrary instances for Poly1305 -------------
instance Arbitrary Poly1305.R where
  arbitrary = genEncodable

instance Arbitrary Poly1305.S where
  arbitrary = genEncodable

instance Arbitrary Poly1305.Poly1305 where
  arbitrary = genEncodable

instance Arbitrary (Key Poly1305) where
  arbitrary = Poly1305.Key <$> arbitrary <*> arbitrary

---------------- Arbitrary instances for  X25519 ------------------

instance Arbitrary (Private X25519) where
  arbitrary = genEncodable

instance Arbitrary (Exchange X25519) where
  arbitrary = genEncodable

instance Arbitrary (Secret X25519) where
  arbitrary = genEncodable

genEncodable :: (Encodable a, Storable a) => Gen a
genEncodable = go undefined
  where go :: (Encodable a, Storable a) => a -> Gen a
        go x = unsafeFromByteString . pack <$> vector (fromEnum $ sizeOf $ pure x)
