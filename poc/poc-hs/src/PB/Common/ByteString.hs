module PB.Common.ByteString
  ( PBByteString (..),
    PBString,
    PBBytes,
    RsString,
    RsBytes,
  )
where

import Data.ByteString qualified as BS
import Data.ByteString.Internal (ByteString (..))
import Data.Word
import Foreign
import Foreign.C.Types
import PB.Common.Types

data PBByteString repr where
  PBByteString :: PBByteStringRepr repr => {-# UNPACK #-} !BS.ByteString -> PBByteString repr

class PBByteStringRepr repr

data PBString

instance PBByteStringRepr PBString

data PBBytes

instance PBByteStringRepr PBBytes

data RsString

foreign import ccall
  rs_string_get_ptr :: Ptr RsString -> IO (Ptr Word8)

foreign import ccall
  rs_string_get_len :: Ptr RsString -> IO CSize

foreign import ccall
  rs_bytes_get_ptr :: Ptr RsBytes -> IO (Ptr Word8)

foreign import ccall
  rs_bytes_get_len :: Ptr RsBytes -> IO CSize

foreign import ccall
  rs_string_copy :: Ptr Word8 -> CInt -> IO (Ptr RsString)

instance RsDrop RsString where
  dropRs = rs_string_drop

instance RsFFI RsString (PBByteString PBString) where
  fromRs rs = do
    ptr <- newForeignPtr_ =<< rs_string_get_ptr rs
    len <- fromIntegral <$> rs_string_get_len rs
    pure . PBByteString $ BS ptr len

  toRs (PBByteString hs) = do
    let BS ptr len = hs
    withForeignPtr ptr \ptr -> rs_string_copy ptr (fromIntegral len)

data RsBytes

foreign import ccall
  rs_bytes_copy :: Ptr Word8 -> CInt -> IO (Ptr RsBytes)

foreign import ccall
  rs_string_drop :: Ptr RsString -> IO ()

foreign import ccall
  rs_bytes_drop :: Ptr RsBytes -> IO ()

instance RsDrop RsBytes where
  dropRs = rs_bytes_drop

instance RsFFI RsBytes (PBByteString PBBytes) where
  fromRs rs = do
    ptr <- newForeignPtr_ =<< rs_bytes_get_ptr rs
    len <- fromIntegral <$> rs_bytes_get_len rs
    pure . PBByteString $ BS ptr len

  toRs (PBByteString hs) = do
    let BS ptr len = hs
    withForeignPtr ptr \ptr -> rs_bytes_copy ptr (fromIntegral len)
