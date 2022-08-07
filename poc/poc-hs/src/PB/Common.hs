module PB.Common
  ( Streaming,
    PBByteString (..),
    PBString,
    PBBytes,
    RsString,
    RsBytes,
    GRPCStatus (..),
    RsFFI (..),
    RsHashMap,
    fromRsHashMap,
    toRsHashMap,
    fromRsVec,
    toRsVec,
  )
where

import Conduit qualified as C
import Data.ByteString qualified as BS
import Data.ByteString.Internal (ByteString (..))
import Data.Functor
import Data.Map.Strict qualified as Map
import Data.Word
import Foreign
import Foreign.C.Types

type Streaming a = C.ConduitT () a IO ()

data PBByteString repr where
  PBByteString :: PBByteStringRepr repr => {-# UNPACK #-} !BS.ByteString -> PBByteString repr

class PBByteStringRepr repr

data PBString

instance PBByteStringRepr PBString

data PBBytes

instance PBByteStringRepr PBBytes

data GRPCStatus = GRPCStatus
  { code :: CInt,
    message :: BS.ByteString,
    details :: BS.ByteString
  }

class RsFFI rst hst where
  fromRs :: Ptr rst -> IO hst
  toRs :: hst -> IO (Ptr rst)

data RsString

data RsBytes

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

foreign import ccall
  rs_bytes_copy :: Ptr Word8 -> CInt -> IO (Ptr RsBytes)

instance RsFFI RsString (PBByteString PBString) where
  fromRs rs = do
    ptr <- newForeignPtr_ =<< rs_string_get_ptr rs
    len <- fromIntegral <$> rs_string_get_len rs
    pure . PBByteString $ BS ptr len

  toRs (PBByteString hs) = do
    let BS ptr len = hs
    withForeignPtr ptr \ptr -> rs_string_copy ptr (fromIntegral len)

instance RsFFI RsBytes (PBByteString PBBytes) where
  fromRs rs = do
    ptr <- newForeignPtr_ =<< rs_bytes_get_ptr rs
    len <- fromIntegral <$> rs_bytes_get_len rs
    pure . PBByteString $ BS ptr len

  toRs (PBByteString hs) = do
    let BS ptr len = hs
    withForeignPtr ptr \ptr -> rs_bytes_copy ptr (fromIntegral len)

data RsHashMap rsk rsv

fromRsHashMap :: forall rsk rsv hsk hsv. (RsFFI rsk hsk, RsFFI rsv hsv) => Ptr (RsHashMap rsk rsv) -> IO (Map.Map hsk hsv)
fromRsHashMap = undefined

toRsHashMap :: forall hsk hsv rsk rsv. (RsFFI rsk hsk, RsFFI rsv hsv) => Map.Map hsk hsv -> IO (Ptr (RsHashMap rsk rsv))
toRsHashMap = undefined

fromRsVec :: RsFFI rst hst => Ptr (Ptr rst) -> Int -> IO [hst]
fromRsVec (ptr :: Ptr (Ptr rst)) len =
  sequence $ [0 .. len] <&> \i -> fromRs @rst (ptr `plusPtr` i)

toRsVec :: RsFFI rst hst => [hst] -> IO (Ptr (Ptr rst), CSize)
toRsVec = undefined
