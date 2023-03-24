module Raaz.KeyExchange.X25519Spec where


import Tests.Core
import Raaz.KeyExchange.X25519.Internal
import Raaz.Curve25519.Field


import Raaz.Random

randomClamping :: Spec
randomClamping = it "randomly generated private key is clamped"
       $ checkClamped `shouldReturn` True
  where randR :: RandomState -> IO (Private X25519)
        randR = random
        checkClamped = withRandomState (fmap isClamped . randR)

spec :: Spec
spec = do
  describe "private key" $ do
    basicEndianSpecs (undefined :: Private X25519)
    randomClamping

  describe "exchange data" $
    basicEndianSpecs (undefined :: Exchange X25519)

  describe "shared secret" $
    basicEndianSpecs (undefined :: Secret X25519)


isClamped :: Private X25519 -> Bool
isClamped (Private w) = check (word256ToInteger w)
  where check x = and [ isCleared 0 x
                      , isCleared 1 x
                      , isCleared 2 x
                      , isSet 254 x
                      , isCleared 255 x
                      ]
        isCleared n x = testBit (complementBit x n) n
        isSet = flip testBit

-------------------------------------------------------------------------------
--    X25519 using the Integer Type
--
-- WARNING: only to be used for testing, it is slow and unsafe.
--
