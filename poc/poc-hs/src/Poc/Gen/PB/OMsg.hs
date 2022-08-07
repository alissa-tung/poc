module Poc.Gen.PB.OMsg where

import PB.Common

data IMsg
  = DoubleMsg Double
  | BoolMsg Bool
  | StringMsg (PBByteString PBByteStringReprString)
  | BytesMsg (PBByteString PBByteStringReprBytes)
