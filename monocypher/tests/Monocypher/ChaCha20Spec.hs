{-# LANGUAGE ForeignFunctionInterface #-}
-- | These tests compare the implementation of chacha20 of raaz and
-- that of monocypher. The only challenge here is to adjust the nounce
-- as raaz implements the ieft variants where as monocypher implements
-- the djb variant. The tests therefore works only for nounces that
-- are 64-bit wide.
module Monocypher.ChaCha20Spec where


import qualified Data.ByteString as BS
import           Data.ByteString.Internal (unsafeCreate)
import           Data.ByteString.Unsafe   (unsafeUseAsCStringLen)
import qualified Foreign.Storable as Storable
import           Foreign.Ptr
import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.Marshal.Alloc

import           Raaz.Core
import qualified Raaz.Encrypt.ChaCha20  as ChaCha20
import qualified Raaz.Encrypt.XChaCha20 as XChaCha20
import           Tests.Core

foreign import ccall unsafe
      crypto_ietf_chacha20 :: Ptr Word8  -- cipher text
                           -> Ptr CChar  -- plain text
                           -> Int        -- Size
                           -> Ptr Word8  -- key
                           -> Ptr Word8  -- nounce
                           -> IO ()

foreign import ccall unsafe
    crypto_xchacha20 :: Ptr Word8  -- cipher text
                     -> Ptr CChar  -- plain text
                     -> Int        -- Size
                     -> Ptr Word8  -- key
                     -> Ptr Word8  -- nounce
                     -> IO ()

monocypher_chacha20_io :: Key ChaCha20
                       -> Nounce ChaCha20
                       -> Ptr Word8
                       -> CStringLen     -- plain text
                       -> IO ()
monocypher_chacha20_io k n cPtr (pPtr, l)
  = allocaBytes kSize
    $ \ kptr ->
        allocaBytes nSize $ \ nptr ->
                              do store (castPtr kptr) k
                                 store (castPtr nptr) n
                                 -- The 64 bit nounce is at an offset
                                 -- of 4-bytes recall that the nounce
                                 -- has its top 4-bytes as zeros.
                                 crypto_ietf_chacha20 cPtr pPtr l kptr nptr
  where kSize = Storable.sizeOf (undefined :: Key ChaCha20)
        nSize = Storable.sizeOf (undefined :: Nounce ChaCha20)

monocypher_xchacha20_io :: Key XChaCha20
                        -> Nounce XChaCha20
                        -> Ptr Word8
                        -> CStringLen     -- plain text
                        -> IO ()
monocypher_xchacha20_io k n cPtr (pPtr, l)
  = allocaBytes kSize
    $ \ kptr ->
        allocaBytes nSize $ \ nptr ->
                              do store (castPtr kptr) k
                                 store (castPtr nptr) n
                                 crypto_xchacha20 cPtr pPtr l kptr nptr
  where kSize = Storable.sizeOf (undefined :: Key XChaCha20)
        nSize = Storable.sizeOf (undefined :: Nounce XChaCha20)

monocypher_chacha20_encrypt :: Key ChaCha20
                            -> Nounce ChaCha20
                            -> ByteString
                            -> ByteString
monocypher_chacha20_encrypt k n bs = unsafeFromByteString $ unsafeCreate l creator
  where l = BS.length bs
        creator = unsafeUseAsCStringLen bs . monocypher_chacha20_io k n


monocypher_xchacha20_encrypt :: Key XChaCha20
                             -> Nounce XChaCha20
                             -> ByteString
                             -> ByteString
monocypher_xchacha20_encrypt k n bs = unsafeFromByteString $ unsafeCreate l creator
  where l = BS.length bs
        creator = unsafeUseAsCStringLen bs . monocypher_xchacha20_io k n

spec :: Spec
spec = do prop "monocypher vs raaz - chacha20" $
            \ k n x ->  monocypher_chacha20_encrypt k n x `shouldBe` ChaCha20.encrypt k n x

          prop "monocypher vs raaz - xchacha20" $
            \ k n x -> monocypher_xchacha20_encrypt k n x `shouldBe` XChaCha20.encrypt k n x