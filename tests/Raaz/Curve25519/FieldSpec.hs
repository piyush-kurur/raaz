{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Raaz.Curve25519.FieldSpec where


import Data.List as List
import Data.Bits
import Data.Vector.Unboxed hiding (sum)
import Foreign.Ptr (castPtr)
import System.IO.Unsafe ( unsafePerformIO )

import Tests.Core
import Raaz.KeyExchange.X25519.Internal
import Raaz.Curve25519.Field
import Raaz.Core.Types.Internal as TI
import Raaz.Verse.Curve25519Test.C.Portable


-- | Store and the load the given value.
storeLoadLimb :: GF -> IO GF
storeLoadLimb = doit . toBits256
  where doit w = allocaBuffer (sizeOf $ pure w) (fmap (fromInteger . word256ToInteger) . runStoreLoad w)
        runStoreLoad w ptr = do poke ptr w
                                verse_gf25519_load_store (castPtr ptr)
                                peek ptr

type Limbs = Tuple 10 Word64

clearTopBit :: Limbs -> Limbs
clearTopBit = TI.map (flip clearBit 63)

limbTransform :: (Ptr Limbs -> IO ()) -> Limbs -> Limbs
limbTransform action lmbs = unsafePerformIO doit
  where doit = allocaBuffer (sizeOf $ pure lmbs) $ runStoreLoad lmbs
        runStoreLoad ls ptr = do poke ptr ls
                                 action ptr
                                 peek ptr

propagate :: Limbs -> Limbs
propagate = limbTransform verse_gf25519_propagate

reduce :: Limbs -> Limbs
reduce  = limbTransform verse_gf25519_reduce


type Packed = W256

pos :: Int -> Int
pos n = 51 * q + 26 * r
  where (q,r) = n `quotRem` 2

len :: Int -> Int
len n = pos (n+1) - pos n


allOnes :: Word64
allOnes = complement 0

maxLimb :: Int -> Word64
maxLimb i = allOnes `shiftR` (64 - len i)

maxLimbs :: Limbs
maxLimbs = unsafeFromList [ maxLimb i | i <- [0..9] ]

maxLimbsP :: Limbs
maxLimbsP = unsafeFromList $ [ maxLimb i | i <- [0..8] ] List.++ [oneA]
  where oneA = allOnes `shiftR` (64 - len 9 - 1)

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

  describe "propagate" $ do
    it "will given the same element for limbs with all but top bit being one " $
      let ones = clearBit (complement (0 :: Word64)) 63
          onesTup = unsafeFromList (List.replicate 10 ones) in
        limbsToGF (propagate onesTup) `shouldBe` limbsToGF onesTup

    prop "should not change the field element" $ \ lmbs ->
      let ls = clearTopBit lmbs in
        limbsToGF (propagate ls) `shouldBe` limbsToGF ls

  describe "reduce" $ do
    it "will give a field element for maxLimb" $
      limbsToInteger (reduce maxLimbs) `shouldBe` unsafeToInteger (limbsToGF maxLimbs)

    it "will give the same field element for maxLimbs Plus additional bit" $
      limbsToInteger (reduce maxLimbsP)  `shouldBe` unsafeToInteger (limbsToGF maxLimbsP)

    prop "should give reduced values for all limbs" $ \ lmbs ->
      limbsToInteger (reduce lmbs) `shouldBe` unsafeToInteger (limbsToGF lmbs)

  describe "field arithmetic" $ do
    prop "addition" $ \ b c -> addition b c `shouldBe` (b + c)
    prop "subtraction" $ \ b c -> subtraction b c `shouldBe` (b - c)
    prop "multiplication" $ \ b c -> multiplication b c `shouldBe` (b * c)
