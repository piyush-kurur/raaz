module Raaz.KeyExchange.X25519Spec where

import System.IO.Unsafe ( unsafePerformIO )

import Tests.Core
import Raaz.KeyExchange.X25519.Internal
import Raaz.Curve25519.Field
import Raaz.Random

import X25519.Implementation

value :: (Internals -> IO a) -> a
value action = unsafePerformIO $ withMemory action

exchange :: Private X25519 -> Exchange X25519
exchange key = value $ \ mem -> do
  initialise key mem
  setOwnExchange mem
  extract mem

secret :: Private X25519 -> Exchange X25519 -> Secret X25519
secret key x = value $ \ mem -> do
  initialise key mem
  setSharedSecret x mem
  extract mem



randomClamping :: Spec
randomClamping = it "randomly generated private key is clamped"
       $ checkClamped `shouldReturn` True
  where randR :: RandomState -> IO (Private X25519)
        randR = random
        checkClamped = withRandomState (fmap isClamped . randR)

secretCheck :: Private X25519 -> Private X25519 -> Bool
secretCheck kA kB = secret kA xB == secret kB xA
  where xA = exchange kA
        xB = exchange kB


spec :: Spec
spec = do
  describe "private key" $ do
    basicEndianSpecs (undefined :: Private X25519)
    randomClamping
    prop "integer clamping" $ \ i -> isClampedInteger (clamp i)

  describe "exchange data" $ do
    basicEndianSpecs (undefined :: Exchange X25519)
    prop "useless" $ (2 :: Int) `shouldBe` 3
    prop "exchange data computation with reference" $ \ k ->
      exchange k `shouldBe` x25519Public k
  describe "shared secret" $ do
    basicEndianSpecs (undefined :: Secret X25519)
    prop "same shared secret for a key pair" $ \ kA kB ->
      secret kA (exchange kB) `shouldBe` secret kB (exchange kA)



isClamped :: Private X25519 -> Bool
isClamped (Private w256) = isClampedInteger $ word256ToInteger w256

isClampedInteger :: Integer -> Bool
isClampedInteger x = and [ isCleared 0
                         , isCleared 1
                         , isCleared 2
                         , isSet 254
                         , isCleared 255
                         ]
  where isCleared n = testBit (complementBit x n) n
        isSet = testBit x

clamp :: Integer -> Integer
clamp = setB 254 . clearB 255 . clearB 2 . clearB 1 . clearB 0
  where setB = flip setBit
        clearB = flip clearBit

-------------------------------------------------------------------------------
--    X25519 using the Integer Type
--
-- WARNING: only to be used for testing, it is slow and unsafe.
--

-- | The projective point X and Z coordinate
type PPoint = (GF, GF)


-- The curve equation is given by y^2 = x^3 + A x^2 + x. This is the
-- constant A in the above equation
aA :: GF
aA = 486662

affine :: PPoint -> GF
affine (x,z) = x * inverse z


double :: PPoint -> PPoint
double (x,z) = (x2,z2)
  where u  = x*x - z*z
        x2 = u * u
        z2 = 4 * x * z * (x*x + aA * x * z + z*z)


add :: GF -> PPoint -> PPoint -> PPoint
add xB (x1,z1) (x2,z2) = (x,z)
  where u = x1 * x2 - z1 * z2
        v = x1 * z2 - x2 * z1
        x = u * u
        z = xB * v * v


smult :: Integer -> GF -> GF
smult n xB = affine $ powGen (add xB) double infty p n
  where infty = (1,0)
        p     = (xB,1)

x25519 :: Private X25519 -> Exchange X25519 -> Exchange X25519
x25519 (Private prv256) (Exchange x256) = Exchange $ toWord256 $ smult n xB
  where n     = clamp       $ word256ToInteger prv256
        xB    = fromInteger $ word256ToInteger x256


x25519Public :: Private X25519 -> Exchange X25519
x25519Public pv = x25519 pv base
  where base = Exchange $ toWord256 $ 9
