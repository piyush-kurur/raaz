{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|

Contexts are useful when computing message digests of streaming input,
i.e needs incremental processing. It consists of the internal state of the
primitive, a buffer for intermediate data, and a memory cell for keeping
track of the total number of data that is remaining.

-}

module Context ( Cxt(..)
               , cxtSize
               , cxtBlockCount
               , unsafeSetCxtEmpty
               , unsafeSetCxtFull
               , unsafeGenerateBlocks
               , unsafeConsumeBlocks
               , unsafeWriteTo
               , unsafeFillFrom
               , unsafeUpdate
               , unsafeFinalise
               ) where

import GHC.TypeLits

import Raaz.Core
import Implementation
import Buffer

----------------- Contexts ------------------------------
-- | There are two cases where incremental processing of bytes are
--   desired.
--
-- 1. For incremental message digest or message auth computation. We call this
--    the hashing mode.
--
-- 2. For CSPRG. We call this the csprg mode.
--
-- In both cases, we need a buffer, the internal memory state and a
-- count of how much data is remaining in the context. This is
-- captured by the Cxt type.

data Cxt n = Cxt { cxtInternals       :: Internals
                 , cxtBuf             :: Buffer n
                 , cxtAvailableBytes  :: MemoryCell (BYTES Int)
                 }

instance KnownNat n => Memory (Cxt n) where
  memoryAlloc     = Cxt <$> memoryAlloc <*> memoryAlloc <*> memoryAlloc
  unsafeToPointer = unsafeToPointer . cxtInternals

{--

-- Unfortunately this require UndecidableInstances so we suppress
-- these instances.

instance (KnownNat n, Initialisable Internals v) =>
  Initialisable (Cxt n) v where
  initialise v cxt@Cxt{..} = initialise v cxtInternals
                             >> unsafeSetCxtEmpty cxt

instance (KnownNat n, Extractable Internals v) =>
  Extractable (Cxt n) v where
  extract = extract . cxtInternals

--}
-- | Gives the number of blocks that can fit in the context.
cxtBlockCount :: KnownNat n => Proxy (Cxt n) -> BlockCount Prim
cxtBlockCount = bufferSize . fmap cxtBuf

-- | Gives the size of the context buffer
cxtSize :: KnownNat n => Proxy (Cxt n) -> BYTES Int
cxtSize = inBytes . cxtBlockCount

-- | Total valid bytes (either generated or read) that is available at
-- the front of the buffer.
getCxtBytes :: KnownNat n => Cxt n -> IO (BYTES Int)
getCxtBytes = extract . cxtAvailableBytes

-- | Set the current number of bytes.
setBytes :: BYTES Int -> Cxt n -> IO ()
setBytes nbytes = initialise nbytes . cxtAvailableBytes

-- | Set the context to the empty state.
unsafeSetCxtEmpty :: Cxt n -> IO ()
unsafeSetCxtEmpty = initialise (0 :: BYTES Int) . cxtAvailableBytes

-- | Set the context to the full state.
unsafeSetCxtFull :: KnownNat n => Cxt n -> IO ()
unsafeSetCxtFull cxt@Cxt{..} = initialise (cxtSize $ pure cxt) cxtAvailableBytes

------------------ NOTES ----------------------------------------------
--
-- There is a nice duality between the usage of context in the hashing
-- mode as opposed to csprg mode.
--
-- [Hashing mode:] Bytes are consumed by the block processor. For this
-- bytes needs to be supplied from the outside world.
--
-- [CSPRG mode:] Bytes are generated by the block processor. The
-- context supplies these bytes to the out side world.
--
-- You can see this duality reflected in the functions that exists in
-- both the case.

------------------  Generating/Consuming blocks ------------------------
--
-- The first visible duality in the two modes is in the way the two
-- primitives process blocks.
--
-- [HASH mode:] Block compressor should be called when the context
-- buffer is full and, as a result, should leave the context buffer
-- empty.
--
-- [CSPRG mode:] Block generator should be called when the context
-- buffer is empty and, as a result, should leave the context buffer full


-- | Process the entire buffer of the context using the given action.
unsafeProcessBlocks :: KnownNat n
                    => (BufferPtr -> BlockCount Prim -> Internals -> IO ())
                    -> Cxt n
                    -> IO ()
unsafeProcessBlocks action Cxt{..} = withBufferPtr action cxtBuf cxtInternals

-- | Typically used in CSPRG mode, this combinator generates blocks to
-- fill the context buffer. All the current bytes in the context gets
-- overwritten and hence is an unsafe operation. The result of this
-- combinator is a context that is filled with generated bytes ready
-- to be given out.
unsafeGenerateBlocks :: KnownNat n
                     => (BufferPtr -> BlockCount Prim -> Internals -> IO ())
                     -- ^ Blocks generator
                     -> Cxt n
                     -> IO ()
unsafeGenerateBlocks genBlocks cxt = unsafeProcessBlocks genBlocks cxt >> unsafeSetCxtFull cxt


-- | Typically used in the Hashing mode, this combinator assumes that
-- the context is full and consumes these blocks. This action does not
-- check whether the context is full and hence is unsafe. The result
-- of this action is an empty context ready to receive further bytes.
unsafeConsumeBlocks :: KnownNat n
                    => (BufferPtr -> BlockCount Prim -> Internals -> IO ())
                    -> Cxt n
                    -> IO ()
unsafeConsumeBlocks action cxt = unsafeProcessBlocks action cxt >> unsafeSetCxtEmpty cxt

--------------------------- DANGEROUS CODE ---------------------------------------
--
-- The picture below summarises the state of the buffer.
--
--
-- >  sptr                endPtr
-- >   |                  |
-- >   |<----available--->|<----remaining ----------------->|
--     V                  V                                 V
-- >   +----------------------------------------------------+
-- >   |                  |                                 |
-- >   +----------------------------------------------------+
-- >   ^                                                    ^
-- >   |<------------ buffer length (l)  ------------------>|
--
--

-- | Starting pointer of the context buffer.
startPtr :: Cxt n -> Ptr Word8
startPtr = unsafeWithPointerCast  id . unsafeGetBufferPointer . cxtBuf


-- > startPtr             srcPtr
-- >   |<------------------- available -------------------->|
-- >   |                  |                                 |
-- >   |<----leftover --->|<---- satisfy  ----------------->|
-- >   V                  V                                 V
-- >   +----------------------------------------------------+
-- >   |                  |                                 |
-- >   +----------------------------------------------------+
-- >   ^                                                    ^
-- >   |<------------ buffer length (l)  ------------------>|
--
--


-- | This action writes out to the given pointer buffer, bytes from
-- the context. The copy of the bytes written in the context buffer is
-- wiped so that looking at the context it is impossible to predict
-- what was written out. The return value is the actual number of
-- bytes written out which may be less than the amount demanded.
unsafeWriteTo :: KnownNat n
              => BYTES Int         -- ^ How many bytes to send to destination.
              -> Dest (Ptr Word8)  -- ^ destination pointer
              -> Cxt n
              -> IO (BYTES Int)
unsafeWriteTo req dbuf cxt = do
  ava <- getCxtBytes cxt           -- bytes available in the context
  let satisfy  = min req ava    -- how much of the demand can be satisfied.
      leftover = ava - satisfy  -- the leftover bytes.
      srcPtr   = startPtr cxt `movePtr` leftover
    in do memcpy dbuf (source srcPtr) satisfy -- transfer the actual bytes
          wipeMemory srcPtr satisfy           -- wipe the copy
          setBytes leftover cxt
          return satisfy



-- > startPtr             destPtr
-- >   |<------------------- bufSize   -------------------->|
-- >   |                  |                                 |
-- >   |<----available--->|<---- vacant   ----------------->|
-- >   V                  V                                 V
-- >   +----------------------------------------------------+
-- >   |                  |                                 |
-- >   +----------------------------------------------------+
-- >   ^                                                    ^
-- >   |<------------ buffer length (l)  ------------------>|
--
--

-- | This action fills from the given byte source to the context.
unsafeFillFrom :: (KnownNat n, ByteSource src)
               => src
               -> Cxt n
               -> IO (FillResult src)
unsafeFillFrom src cxt = do
  ava <- getCxtBytes cxt           -- bytes available in the context
  let vacant  = cxtSize (pure cxt) - ava
      destPtr = startPtr cxt `movePtr` ava
      srcExhausted trfed = setBytes (ava + trfed) cxt >> return (Exhausted trfed)
      srcRemaining remSrc = unsafeSetCxtFull cxt >> return (Remaining remSrc)
    in fillBytes vacant src destPtr >>= withFillResult srcRemaining srcExhausted


-- | Starting with an empty context, run the given action on the src
-- reading a full buffer at a time. Ends when the src is
-- exhausted. The last chunk of bytes that is read is not processed
-- and is left in the buffer for later processing when more bytes are
-- added or when finalising the context.
unsafeContinue :: (KnownNat n, ByteSource src)
               => (BufferPtr -> BlockCount Prim -> Internals -> IO ())
               -> src
               -> Cxt n
               -> IO ()
unsafeContinue action src cxt = processChunks actFilled lastChunk src bufPtr bufSize
  where actFilled = unsafeProcessBlocks action cxt
        lastChunk nbytes = setBytes nbytes cxt
        bufPtr           = startPtr cxt
        bufSize          = cxtSize $ pure cxt


-- | Update the context with data coming from the byte source. Used
-- typically in the digest mode.
unsafeUpdate :: (KnownNat n, ByteSource src)
             => (BufferPtr -> BlockCount Prim -> Internals -> IO ())
             -> src
             -> Cxt n
             -> IO ()
unsafeUpdate action src cxt =
  unsafeFillFrom src cxt >>= withFillResult process doNothing
  where doNothing      = const $ return ()
        process remSrc = unsafeConsumeBlocks action cxt >> unsafeContinue action remSrc cxt

-- | Finalise the context with the last chunk of data.
unsafeFinalise :: KnownNat n
               => (BufferPtr -> BYTES Int -> Internals -> IO ())
               -> Cxt n
               -> IO ()
unsafeFinalise action cxt@Cxt{..} = do
  ava <- getCxtBytes cxt
  unsafeWithBufferPtr action cxtBuf ava cxtInternals
