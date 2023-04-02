{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Raaz.Curve25519.FieldSpec where


import Data.Vector.Unboxed hiding (sum)
import Foreign.Ptr (castPtr)
import System.IO.Unsafe ( unsafePerformIO )

import Tests.Core
import Raaz.KeyExchange.X25519.Internal
import Raaz.Curve25519.Field
import Raaz.Verse.Curve25519Test.C.Portable


-- | Store and the load the given value.
storeLoadLimb :: GF -> IO GF
storeLoadLimb = doit . toBits256
  where doit w = allocaBuffer (sizeOf $ pure w) (fmap (fromInteger . word256ToInteger) . runStoreLoad w)
        runStoreLoad w ptr = do poke ptr w
                                verse_gf25519_load_store (castPtr ptr)
                                peek ptr

type Limbs = Tuple 10 Word64
type Packed = W256

pos :: Int -> Int
pos n = 51 * q + 26 * r
  where (q,r) = n `quotRem` 2

limbsToInteger :: Limbs -> Integer
limbsToInteger lmb = sum [ w i | i <- [0..9] ]
  where w i   = toInteger (unsafeToVector lmb ! i) * 2^(pos i)

limbsToGF :: Limbs -> GF
limbsToGF = fromInteger . limbsToInteger


data OperMem = OperMem { resultCell :: MemoryCell Limbs
                       , arg1Cell   :: MemoryCell Packed
                       , arg2Cell   :: MemoryCell Packed
                       }

instance Memory OperMem where
  memoryAlloc = OperMem <$> memoryAlloc <*> memoryAlloc <*> memoryAlloc
  unsafeToPointer = unsafeToPointer . resultCell

oper2 :: (Ptr Limbs -> Ptr W256 -> Ptr W256 -> IO ()) -> GF -> GF -> OperMem -> IO Limbs
oper2 f b c mem = do initialise (toBits256 b :: Packed) $ arg1Cell mem
                     initialise (toBits256 c :: Packed) $ arg2Cell mem
                     let aPtr = unsafeGetCellPointer $ resultCell mem
                         bPtr = unsafeGetCellPointer $ arg1Cell mem
                         cPtr = unsafeGetCellPointer $ arg2Cell mem
                       in f aPtr bPtr cPtr
                     extract $ resultCell mem

operation :: (Ptr Limbs -> Ptr W256 -> Ptr W256 -> IO ()) -> GF -> GF -> GF
operation f b c = unsafePerformIO $ withMemory (fmap limbsToGF . oper2 f b c)

addition :: GF -> GF -> GF
addition = operation verse_gf25519_addition

subtraction :: GF -> GF -> GF
subtraction = operation verse_gf25519_minus

multiplication :: GF -> GF -> GF
multiplication = operation verse_gf25519_multiplication

{-
operA :: (Ptr Limbs -> Ptr W256 -> Ptr W256 -> IO ()) -> GF -> GF -> OperMem -> IO Limbs
operA f b c mem = do initialise (toBits256 b :: W256) $ arg1Cell mem
                     initialise (toBits256 c :: W256) $ arg2Cell mem
                     let aPtr = unsafeGetCellPointer $ resultCell mem
                         bPtr = unsafeGetCellPointer $ arg1Cell mem
                         cPtr = unsafeGetCellPointer $ arg2Cell mem
                       in f aPtr bPtr cPtr
                     extract $ resultCell mem

-}



spec :: Spec
spec = do

  describe "load/store limbs" $ do
    prop "load followed by store should give the same value" $
      \ gf -> storeLoadLimb gf `shouldReturn` gf

  describe "field arithmetic" $ do
    prop "addition" $ \ b c -> addition b c `shouldBe` (b + c)
    prop "subtraction" $ \ b c -> subtraction b c `shouldBe` (b - c)
    prop "multiplication" $ \ b c -> multiplication b c `shouldBe` (b * c)
