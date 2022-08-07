module Poc.Gen.PB where

import Data.Map.Strict qualified as Map
import PB.Common
import Poc.Gen.PB.OMsg qualified as OMsg

data PocService = PocService
  { pocEcho ::
      OMsg ->
      Either OMsg GRPCStatus,
    serverStreaming ::
      PocMsg ->
      Streaming (Either PocMsg GRPCStatus),
    clientStreaming :: Streaming PocMsg -> Either PocMsg GRPCStatus,
    bidiStreaming :: Streaming PocMsg -> Streaming (Either PocMsg GRPCStatus)
  }

data OMsg = OMsg
  { xs :: [OMsg.IMsg],
    ys :: Map.Map (PBByteString PBByteStringReprString) (PBByteString PBByteStringReprString)
  }

newtype PocMsg = PocMsg
  { xs :: PBByteString PBByteStringReprString
  }
