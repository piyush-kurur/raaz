{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE DataKinds            #-}

module Raaz.Curve25519.Field ( GF
                             , normalise
                             , toBits256
                             , word256ToInteger
                             , powGen
                             , inverse
                             , unsafeToInteger
                             ) where

import Tests.Core
import qualified Data.Vector.Unboxed as VU
import qualified Data.List  as List

import Raaz.KeyExchange.X25519.Internal

b64 :: Integer
b64 = 2^(64 :: Integer)

b64_2 :: Integer
b64_2 = b64 * b64

b64_3 :: Integer
b64_3 = b64 * b64_2

prime :: Integer
prime =  2^(255 :: Int) - 19


newtype GF = GF Integer deriving (Show, Eq)

word256ToInteger :: B32 -> Integer
word256ToInteger w256 = sum $ List.zipWith (*) ws [1, b64, b64_2, b64_3]
  where ws = List.map toInteger $ VU.toList $ unsafeToVector w256

-- | We should be comparing the bitwise representation.
toBits256 :: (Num a, VU.Unbox a) => GF -> Tuple 4 a
toBits256 (GF x) = unsafeFromList [ fromInteger x
                                  , fromInteger (x `div` b64)
                                  , fromInteger (x `div` b64_2)
                                  , fromInteger (x `div` b64_3)
                                  ]

normalise :: GF -> GF
normalise (GF x) | xp < 0 = GF (prime - xp)
                 | otherwise = GF xp
  where xp = x `mod` prime

binop :: (Integer -> Integer -> Integer) -> GF -> GF -> GF
binop f (GF x) (GF y) = normalise $ GF $ f x y

uniop :: (Integer -> Integer) -> GF -> GF
uniop f (GF x) = normalise $ GF $ f x

instance Num GF where
  (+) = binop (+)
  (*) = binop (*)
  negate = uniop (\ x -> prime - x)
  abs = uniop abs
  signum  = uniop signum
  fromInteger = normalise . GF

instance Arbitrary GF where
  arbitrary = fromInteger <$> arbitrary

-- | Computing generic powering using repeated squaring.
powGen :: (a -> a -> a)
       -> (a -> a)
       -> a
       -> a
       -> Integer
       -> a
powGen mult sq unit a n | n < 0     = error "powGen negative number"
                        | otherwise = go (unit, a) n
  where go (cp, x) m | m == 0          = cp
                     | testBit m 0     = go (mult cp x , sq x) (shiftR m 1)
                     | otherwise       = go (cp, sq x) (shiftR m 1)


-- | Computing the multiplicative inverse in GF.
inverse :: GF -> GF
inverse x = powGen (*) (\ y -> y * y) (1 :: GF) x (prime - 2)


unsafeToInteger (GF x) = x
