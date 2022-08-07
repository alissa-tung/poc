module PB.Common.Containers where

import Data.Foldable
import Data.Functor
import Data.Map.Strict qualified as Map
import Foreign
import Foreign.C.Types
import PB.Common.Types

data RsHashMap rsk rsv

fromRsHashMap :: forall rsk rsv hsk hsv. (RsFFI rsk hsk, RsFFI rsv hsv) => Ptr (RsHashMap rsk rsv) -> IO (Map.Map hsk hsv)
fromRsHashMap = undefined

toRsHashMap :: forall rsk rsv hsk hsv. (RsFFI rsk hsk, RsFFI rsv hsv) => Map.Map hsk hsv -> IO (Ptr (RsHashMap rsk rsv))
toRsHashMap = undefined

fromRsVec :: RsFFI rst hst => Ptr rst -> Int -> IO [hst]
fromRsVec (ptr :: RsFFI rst hst => Ptr rst) len =
  sequence $ [0 .. len] <&> fromRs @rst @hst . plusPtr ptr

toRsVec :: RsFFI rst hst => [hst] -> IO (Ptr rst, CSize)
toRsVec = undefined

data RsVec a

foreign import ccall
  rs_vec_new :: Ptr a -> CSize -> IO (Ptr (RsVec a))

foreign import ccall
  rs_vec_get_ptr :: Ptr (RsVec a) -> IO (Ptr a)

foreign import ccall
  rs_vec_get_len :: Ptr (RsVec a) -> IO CSize

instance RsDrop a => RsDrop (RsVec a) where
  dropRs (xs :: RsDrop a => Ptr (RsVec a)) = do
    ptr <- rs_vec_get_ptr xs
    len <- fromIntegral <$> rs_vec_get_len xs
    traverse_ (dropRs @a . plusPtr ptr) [0 .. len]

instance RsFFI rst hst => RsFFI (RsVec rst) [hst] where
  fromRs (rs :: RsFFI rst hst => Ptr (RsVec rst)) = do
    ptr <- castPtr @_ @rst <$> rs_vec_get_ptr rs
    len <- fromIntegral <$> rs_vec_get_len rs
    fromRsVec @rst @hst ptr len

  toRs (hs :: RsFFI rst hst => [hst]) = do
    (ptr, len) <- toRsVec @rst @hst hs
    rs_vec_new ptr len
