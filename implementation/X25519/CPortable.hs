{-# LANGUAGE DataKinds                  #-}

-- | The portable C-implementation of Poly1305.
module X25519.CPortable
       ( name, primName, description
       , Prim, Internals
       , setOwnExchange, setSharedSecret
       ) where

import Raaz.Core
import X25519.Memory
import Raaz.KeyExchange.X25519.Internal
import Raaz.Verse.Curve25519.C.Portable (verse_x25519_c_portable)

name :: String
name = "libverse-c"

primName :: String
primName = "x25519"

description :: String
description = "X25519 Key exchange based on the Montgomery curve Curve25519"

type Prim                    = X25519
type Internals               = Mem

-- | The generator of curve25519
generator :: Exchange X25519
generator = Exchange $ unsafeFromList [0x09, 0 , 0 , 0]

scalarMul :: (Mem -> MemoryCell (Exchange X25519))
          -> Exchange X25519
          -> Internals
          -> IO ()
scalarMul cellFn point mem =
  do initialise point (cellFn mem)
     let scalarPtr = privatePtr mem
         pointPtr  = w256PtrOf cellFn mem
        in verse_x25519_c_portable scalarPtr pointPtr


setOwnExchange :: Internals -> IO ()
setOwnExchange = scalarMul ownXCell generator

setSharedSecret :: Exchange Prim -- ^ Exchange data from the peer
                -> Internals
                -> IO ()
setSharedSecret = scalarMul peerXCell
