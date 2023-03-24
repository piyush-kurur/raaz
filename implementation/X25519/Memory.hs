{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
-- | This module implements memory elements used for Poly1305
-- implementations.
module X25519.Memory
       ( Mem(..)
       , privatePtr
       , word256PtrOf
       ) where

import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable)
import Raaz.Core
import Raaz.KeyExchange.X25519.Internal

import Raaz.Verse.Curve25519.C.Portable ( verse_curve25519_c_portable_clamp )

-- | Memory cell for
data Mem = Mem { privateCell :: MemoryCell (Private X25519)
               , ownXCell    :: MemoryCell (Exchange X25519)
               , peerXCell   :: MemoryCell (Exchange X25519)
               , secretCell      :: MemoryCell (Secret X25519)
               }

word256PtrOf :: Storable a
             => (Mem -> MemoryCell a)
             -> Mem
             -> Ptr Word256
word256PtrOf cellFn  = castPtr . unsafeGetCellPointer . cellFn

privatePtr :: Mem -> Ptr Word256
privatePtr = word256PtrOf privateCell


clamp :: Mem -> IO ()
clamp mem = verse_curve25519_c_portable_clamp (privatePtr mem) 1

instance Memory Mem where
  memoryAlloc     = Mem <$> memoryAlloc <*> memoryAlloc <*> memoryAlloc <*> memoryAlloc
  unsafeToPointer = unsafeToPointer . privateCell

-- | Initialise private key.
instance Initialisable Mem (Private X25519) where
  initialise prv mem = initialise prv (privateCell mem) >> clamp mem

-- | Extracts own xchange data
instance Extractable Mem (Exchange X25519) where
    extract = extract . ownXCell

-- | Extract shared secret.
instance Extractable Mem (Secret X25519) where
  extract = extract . secretCell

-- | Write access is given to the private key cell.
instance WriteAccessible Mem where
  writeAccess mem          = writeAccess (privateCell mem)
  afterWriteAdjustment mem = do
    afterWriteAdjustment $ privateCell mem
    clamp mem

-- | Read access is given to the shared secret.
instance ReadAccessible Mem where
  readAccess           = readAccess . secretCell
  beforeReadAdjustment = beforeReadAdjustment . secretCell
