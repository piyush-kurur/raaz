{-# LANGUAGE OverloadedStrings #-}

module Raaz.KeyExchange.X25519Spec where

import System.IO.Unsafe ( unsafePerformIO )

import Tests.Core
import Raaz.KeyExchange.X25519.Internal
import Raaz.Curve25519.Field
import Raaz.Random

import X25519.Implementation

value :: (Internals -> IO a) -> a
value action = unsafePerformIO $ withMemory action

public :: Private X25519 -> Exchange X25519
public key = value $ \ mem -> do
  initialise key mem
  setPublic mem
  extract mem

secret :: Private X25519 -> Exchange X25519 -> Secret X25519
secret key x = value $ \ mem -> do
  initialise key mem
  setSecret x mem
  extract mem




randomClamping :: Spec
randomClamping = it "randomly generated private key is clamped"
       $ checkClamped `shouldReturn` True
  where randR :: RandomState -> IO (Private X25519)
        randR = random
        checkClamped = withRandomState (fmap isClamped . randR)

checkPublic :: (Private X25519 -> Exchange X25519) -> Private X25519 -> Exchange X25519 -> Spec
checkPublic  pkeyFun prv ex = it message $ pkeyFun prv `shouldBe` ex
  where message = unwords [ "public key associated with"
                          , shortened $ show prv
                          , "should be"
                          , shortened $ show ex
                          ]

checkShared :: (Private X25519 -> Exchange X25519)
            -> (Private X25519 -> Exchange X25519 -> Secret X25519)
            -> Private X25519
            -> Private X25519
            -> Secret X25519
            -> Spec
checkShared pkey shrdSec kA kB sAB =
  let msg = unwords [ "shared secret with private key"
                    , shortened $ show kA
                    , "public key"
                    , shortened $ show (pkey kB)
                    , "should be"
                    , shortened $ show sAB
                    ]
  in
    it msg $ shrdSec kA (pkey kB) `shouldBe` sAB

spec :: Spec
spec = do
  describe "reference implementation" $ do
    prop "infinity is left identity" $ \ x ->
      affine (add x (x,1) infty) `shouldBe` x
    prop "infinity is right identity" $ \ x ->
      affine (add x infty (x,1)) `shouldBe` x

    prop "same shared secret for key pair" $ \ kA kB ->
      secretRef kA (publicRef kB) `shouldBe` secretRef kB (publicRef kA)

    let publicKeyOf = checkPublic publicRef
        sharedKeyOf = checkShared publicRef secretRef
      in do publicKeyOf
              "a8abababababababababababababababababababababababababababababab6b"
              "e3712d851a0e5d79b831c5e34ab22b41a198171de209b8b8faca23a11c624859"

            publicKeyOf
              "c8cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd4d"
              "b5bea823d9c9ff576091c54b7c596c0ae296884f0e150290e88455d7fba6126f"

            publicKeyOf
              "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"
              "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a"

            publicKeyOf
              "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb"
              "de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f"

            sharedKeyOf
              "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"
              "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb"
              "4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742"


  describe "binary conversion" $ do
    describe "private key" $ do
      basicEndianSpecs (undefined :: Private X25519)
      randomClamping

    describe "exchange data" $ do
      basicEndianSpecs (undefined :: Exchange X25519)

    describe "shared secret" $ do
      basicEndianSpecs (undefined :: Secret X25519)
{--}
  describe "vs reference representation" $ do
    prop "public keys should match" $
      \ k -> public k `shouldBe` publicRef k

    prop "shared secret should match" $
      \ kA kB ->
        secret kA (public kB) `shouldBe` secretRef kA (publicRef kB)
--}

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

-- | Point at infinity
infty :: PPoint
infty = (1,0)

-- The curve equation is given by y^2 = x^3 + A x^2 + x. This is the
-- constant A in the above equation
aA :: GF
aA = 486662

affine :: PPoint -> GF
affine (x,z) = x * inverse z

projective :: GF -> PPoint
projective x = (x, 1)

-- Montgomery step

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

type MPair = (PPoint, PPoint)

{-

n = a₀ + 2 a₁ ... + aₖ 2ᵏ

nᵢ = aₖ₋ᵢ + ..... + aₖ 2ᵏ⁻ⁱ  = ⌊ n /2ᵏ⁻ⁱ ⌋

nₖ = n

Pᵢ = nᵢ B
Qᵢ = (nᵢ + 1) B
We have Pᵢ₋₁  Qᵢ-₁

Pᵢ = nᵢ B = (aₖ₋ᵢ + 2 nᵢ₋₁) B  = 2 Pᵢ₋₁ if  aₖ₋ᵢ = 0
                               = Pᵢ-₁ + Qᵢ₋₁

Qᵢ = (1 + nᵢ)B  = (1 + aₖ₋ᵢ + 2 nᵢ-₁) B = Pᵢ-₁ + Qᵢ₋₁ if aₖ₋ᵢ = 0
                                        = 2 Qᵢ₋₁
-}


bitStep :: GF -> Bool -> MPair -> MPair
bitStep xB b (p,q) = if b then (add xB p q, double q)
                     else  (double p, add xB p q)

bits :: Integer -> [Bool]
bits x = [testBit x i | i <- [0..255]]

montgomery :: Integer -> GF -> MPair
montgomery n xB =  steps (infty, projective xB)
  where steps = foldr (.) id $ map (bitStep xB) $ bits n


smult :: Integer -> GF -> GF
smult n = affine . fst . montgomery n

x25519 :: Private X25519 -> Exchange X25519 -> Exchange X25519
x25519 (Private prv256) (Exchange x256) = Exchange $ toBits256 $ smult n xB
  where n     = clamp       $ word256ToInteger prv256
        xB    = fromInteger $ word256ToInteger x256



publicRef :: Private X25519 -> Exchange X25519
publicRef pv = x25519 pv base
  where base = Exchange $ toBits256 $ 9

secretRef :: Private X25519 -> Exchange X25519 -> Secret X25519
secretRef pv ex = case x25519 pv ex of
                    Exchange w -> Secret w
