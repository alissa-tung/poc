module PB.Common.Containers where

import Data.Functor
import Data.Map.Strict qualified as Map
import Foreign
import Foreign.C.Types
import PB.Common.Types

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
