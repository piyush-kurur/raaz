{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE RecordWildCards            #-}
-- | Interface for cryptographically secure random byte generators
-- using a stream cipher for stream expansion.
module Interface
       ( -- * Cryptographically secure randomness.
         -- $randomness$
         RandomState, withRandomState
       , randomByteString
       , fillRandomBytes
       -- ** Generating secure random data
       -- $securerandom$
       , withSecureRandomState
       , randomiseAccess
       , randomiseMemory
       -- ** Types that can be generated randomly
       , Random(..), RandomStorable(..)
       , fillRandom, unsafeFillRandomElements
       -- ** Reseeding from entropy pool
       , reseed
       -- * Information
       , entropySource
       , csprgName, csprgDescription
       ) where

import Control.Applicative
import Control.Monad
import Data.ByteString             ( ByteString             )
import Data.Int
import Data.Vector.Unboxed         ( Unbox )
import Data.Word

import Foreign.Ptr      ( castPtr )
import Foreign.Storable ( Storable         )
import Prelude


import           Raaz.Core
import           Raaz.Core.Memory( Access(..) )
import           PRGenerator ( entropySource, csprgName, csprgDescription, RandomState
                             , fillRandomBytes
                             , withRandomState
                             , withSecureRandomState
                             , reseed
                             )
import qualified Raaz.Primitive.ChaCha20.Internal as ChaCha20
import qualified Raaz.Primitive.Poly1305.Internal as Poly1305
import           Raaz.Verse.Poly1305.C.Portable   ( verse_poly1305_c_portable_clamp)

-- $randomness$
--
-- This module provides cryptographically secure pseudo-random
-- bytes. The state of the csprg is kept track in the memory element
-- `RandomState` and /should/ be run using either the
-- `withRandomState` or `withSecureRandomState`.
--

-- $securerandom$
--
-- When generating sensitive data like long term assymetric keys for
-- one should use the `withSecureRandomState` function so that private
-- information is not swapped to disk. However, merely using it will
-- is not enough. Sensitive data can be swapped from the Haskell heap
-- if we do not pay enough attention. Consider the following code
-- fragment.
--
-- >
-- > -- WARNING: Not safe from swapping
-- > main     :: withSecureRandomState myAction
-- > myAction ::  MySecretMem -> RandomState -> IO ()
-- > myAction mySecMem rstate = do secret <- random rstate
-- >                               initialise secret mysecmem
-- >                               doSomething
--
--
-- The intention of the programmer when using the above code was to
-- randomise the contents of the mySecMem memory element. However,
-- having read the @secret@ as a pure value meant that the value is
-- stored in the Haskell heap. There is know easy way to lock the
-- entire Haskell heap and the garbage collector often moves values
-- around.
--
-- Instead, the correct way to randomise the content is through the
-- `randomiseMemory` combinator. It also explains why the
-- `withSecureRandomState` combinator takes as input an action that
-- has an additional memory argument; without an additional memory
-- element to transfer the random bytes to, there is very little one
-- can do keep the generated pure value from being swapped out.

-- | Subclass of `Storable` which can be randomly generated. It might
-- appear that all instances of the class `Storable` should be
-- be instances of this class, after all we know the size of the
-- element, why not write that many random bytes. In fact, this module
-- provides an `unsafeFillRandomElements` which does that. However, we
-- do not give a blanket definition for all storables because for
-- certain refinements of a given type, like for example, Word8's
-- modulo 10, `unsafeFillRandomElements` introduces unacceptable
-- skews.
class Storable a => RandomStorable a where
  -- | Fill the buffer with so many random elements of type a.
  fillRandomElements :: Int       -- ^ number of elements to fill
                     -> Ptr a     -- ^ The buffer to fill
                     -> RandomState
                     -> IO ()

-- | Fill the given generalised pointer buffer with random elements.
fillRandom :: (RandomStorable a, Pointer ptr)
           => Int
           -> ptr a
           -> RandomState
           -> IO ()
fillRandom n ptr rstate = unsafeWithPointer (\ rawPtr -> fillRandomElements n rawPtr rstate) ptr

-- | Randomise the contents of an access buffer.
randomiseAccess :: Access -> RandomState -> IO ()
randomiseAccess Access{..} rstate = do
  fillRandomBytes accessSize (destination accessPtr) rstate
  accessAfterWrite

-- | Randomise the contents of an accessible memory.
randomiseMemory :: Accessible mem => mem -> RandomState -> IO ()
randomiseMemory mem rstate = mapM_ (flip randomiseAccess rstate) $ confidentialAccess mem

-- TOTHINK:
-- -------
--
-- Do we want to give a default definition like
--
-- > fillRandomElements = unsafeFillRandomElements
--
-- This will make the instance definitions easier for the Storables
-- types that is spread over its entire range. However, it would lead
-- to a lazy definition which will compromise the quality of the
-- randomness.


-- | This is a helper function that has been exported to simplify the
-- definition of a `RandomStorable` instance for `Storable`
-- types. However, there is a reason why we do not give a blanket
-- instance for all instances of the `Storable` class and why this
-- function is unsafe. This function generates a random element of
-- type @a@ by generating @n@ random bytes where @n@ is the size of
-- the elements of @a@. For instances that range the entire @n@ byte
-- space this is fine. However, if the type is actually a refinement
-- of such a type, (consider a @`Word8`@ modulo @10@ for example) this
-- function might generate unacceptable skew in the
-- distribution. Hence this function is prefixed unsafe.
unsafeFillRandomElements :: Storable a  => Int -> Ptr a -> RandomState -> IO ()
unsafeFillRandomElements n ptr = fillRandomBytes totalSz $ destination $ castPtr ptr
  where totalSz = fromIntegral n * sizeOf (getProxy ptr)
        getProxy :: Ptr a -> Proxy a
        getProxy = const Proxy


-- | Generate a random byteString.

randomByteString ::LengthUnit l
                 => l
                 -> RandomState
                 -> IO ByteString
randomByteString l rstate = create l genBS
  where genBS ptr = fillRandomBytes l (destination ptr) rstate

------------------------------- Some instances of Random ------------------------

instance RandomStorable Word8 where
  fillRandomElements = unsafeFillRandomElements

instance RandomStorable Word16 where
  fillRandomElements = unsafeFillRandomElements

instance RandomStorable Word32 where
  fillRandomElements = unsafeFillRandomElements

instance RandomStorable Word64 where
  fillRandomElements = unsafeFillRandomElements

instance RandomStorable Word where
  fillRandomElements = unsafeFillRandomElements

instance RandomStorable Int8 where
  fillRandomElements = unsafeFillRandomElements

instance RandomStorable Int16 where
  fillRandomElements = unsafeFillRandomElements

instance RandomStorable Int32 where
  fillRandomElements = unsafeFillRandomElements

instance RandomStorable Int64 where
  fillRandomElements = unsafeFillRandomElements

instance RandomStorable Int where
  fillRandomElements = unsafeFillRandomElements

instance RandomStorable (Key ChaCha20.ChaCha20) where
  fillRandomElements = unsafeFillRandomElements

instance RandomStorable (Nounce ChaCha20.ChaCha20) where
  fillRandomElements = unsafeFillRandomElements


instance RandomStorable (Key ChaCha20.XChaCha20) where
  fillRandomElements = unsafeFillRandomElements

instance RandomStorable (Nounce ChaCha20.XChaCha20) where
  fillRandomElements = unsafeFillRandomElements


instance RandomStorable Poly1305.R where
  fillRandomElements n ptr state = unsafeFillRandomElements n ptr state >> clamp
    where clamp = verse_poly1305_c_portable_clamp (castPtr ptr) (toEnum n)

instance RandomStorable w => RandomStorable (LE w) where
  fillRandomElements n = fillRandomElements n . lePtrToPtr
    where lePtrToPtr :: Ptr (LE w) -> Ptr w
          lePtrToPtr = castPtr

instance RandomStorable w => RandomStorable (BE w) where
  fillRandomElements n = fillRandomElements n . bePtrToPtr
    where bePtrToPtr :: Ptr (BE w) -> Ptr w
          bePtrToPtr = castPtr

instance (Dimension d, Unbox w, RandomStorable w) => RandomStorable (Tuple d w) where
  fillRandomElements n ptr = fillRandomElements (n * sz ptr) $ tupPtrToPtr ptr
    where sz   :: Dimension d => Ptr (Tuple d w) -> Int
          sz   = dimension' . getProxy
          getProxy :: Ptr (Tuple d w) -> Proxy (Tuple d w)
          getProxy = const Proxy
          tupPtrToPtr ::  Ptr (Tuple d w) -> Ptr w
          tupPtrToPtr = castPtr


--------------------------------- Generating elements randomly ----------------------------

-- | Elements that can be randomly generated.
class Random a where
  random :: RandomState -> IO a

  default random :: RandomStorable a => RandomState -> IO a
  random state = go
    where go =   allocaBuffer (alignedSizeOf $ getProxy go) getIt
          getIt ptr  = fillRandomElements 1 (nextLocation ptr) state >> peekAligned ptr
          getProxy   :: IO a -> Proxy a
          getProxy   = const Proxy


instance (Random a, Random b) => Random (a,b) where
  random state = (,)
                 <$> random state
                 <*> random state
instance (Random a, Random b, Random c) => Random (a,b,c) where
  random state = (,,)
                 <$> random state
                 <*> random state
                 <*> random state


instance (Random a, Random b, Random c, Random d) => Random (a,b,c,d) where
  random state = (,,,)
                 <$> random state
                 <*> random state
                 <*> random state
                 <*> random state


instance (Random a, Random b, Random c, Random d, Random e) => Random (a,b,c,d,e) where
  random state = (,,,,)
                 <$> random state
                 <*> random state
                 <*> random state
                 <*> random state
                 <*> random state

instance Random Word8
instance Random Word16
instance Random Word32
instance Random Word64
instance Random Int8
instance Random Int16
instance Random Int32
instance Random Int64

instance Random (Key ChaCha20.ChaCha20) where
instance Random (Nounce ChaCha20.ChaCha20) where

instance Random (Key ChaCha20.XChaCha20) where
instance Random (Nounce ChaCha20.XChaCha20) where

instance Random Poly1305.R where

instance Random w => Random (LE w) where
  random state = littleEndian <$> random state

instance Random w => Random (BE w) where
  random state = bigEndian <$> random state


instance (Dimension d, Unbox w, Random w) => Random (Tuple d w) where
  random state = generateIO (random state)
