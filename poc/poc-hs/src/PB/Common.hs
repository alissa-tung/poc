module PB.Common
  ( Streaming,
    PBByteString,
    PBByteStringReprString,
    PBByteStringReprBytes,
    GRPCStatus,
  )
where

import Conduit qualified as C
import Data.ByteString qualified as BS

type Streaming a = C.ConduitT () a IO ()

data PBByteString repr where
  PBByteString :: PBByteStringRepr repr => {-# UNPACK #-} !BS.ByteString -> PBByteString repr

class PBByteStringRepr repr

data PBByteStringReprString

instance PBByteStringRepr PBByteStringReprString

data PBByteStringReprBytes

instance PBByteStringRepr PBByteStringReprBytes

-- TODO
data GRPCStatus
