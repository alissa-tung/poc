module Poc.Gen.PB.OMsg where

import PB.Common

data IMsg
  = DoubleMsg Double
  | BoolMsg Bool
  | StringMsg (PBByteString PBString)
  | BytesMsg (PBByteString PBBytes)

data RsIMsg

instance RsFFI RsIMsg IMsg where
  fromRs = undefined
  toRs = undefined
