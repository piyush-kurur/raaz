{-# LANGUAGE FlexibleContexts #-}
module Raaz.Primitive.Util
       ( allocBufferFor
       , processByteSource
       , computeDigest
       , BufferPtr
       ) where

import GHC.TypeLits   (KnownNat)
import Raaz.Core
import Raaz.Primitive.Implementation

-- | The pointer type associated with the buffer used by the
-- implementation.
type BufferPtr = AlignedPointer BufferAlignment

-- | Allocate a buffer for a primitive.
allocBufferFor :: (KnownNat BufferAlignment, MonadAlloc m)
               => BLOCKS Prim
               -> (BufferPtr  -> m a) -> m a
allocBufferFor blks = allocaAligned totalSize
  where totalSize = blks <> additionalBlocks

-- | Process a byte source.
processByteSource :: (KnownNat BufferAlignment, ByteSource src) => src -> MT Internals ()
processByteSource src = allocBufferFor blks $ \ ptr -> do
  processChunks (processBlocks ptr blks) (processLast ptr) src blks (forgetAlignment ptr)
  where blks       = atLeast l1Cache :: BLOCKS Prim

-- | Compute the digest of a message.
computeDigest :: (KnownNat BufferAlignment, ByteSource src)
              => Key Prim -> src -> IO (Digest Prim)
computeDigest key src = insecurely $ do initialise key
                                        processByteSource src
                                        extract
