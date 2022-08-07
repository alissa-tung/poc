module PB.Common.Types where

import Conduit qualified as C
import Foreign

type Streaming a = C.ConduitT () a IO ()

class RsDrop rst where
  dropRs :: Ptr rst -> IO ()

class RsDrop rst => RsFFI rst hst where
  fromRs :: Ptr rst -> IO hst
  toRs :: hst -> IO (Ptr rst)
