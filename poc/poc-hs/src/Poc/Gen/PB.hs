module Poc.Gen.PB where

import Data.Map.Strict qualified as Map
import Foreign
import PB.Common
import Poc.Gen.PB.OMsg qualified as OMsg

data PocService = PocService
  { pocEcho ::
      OMsg ->
      Either GRPCStatus OMsg,
    serverStreaming ::
      PocMsg ->
      Streaming (Either PocMsg GRPCStatus),
    clientStreaming :: Streaming PocMsg -> Either GRPCStatus PocMsg,
    bidiStreaming :: Streaming PocMsg -> Streaming (Either GRPCStatus PocMsg)
  }

data RsOMsg

foreign import ccall
  drop_OMsg :: Ptr RsOMsg -> IO ()

foreign import ccall
  new_OMsg ::
    Ptr (RsVec OMsg.RsIMsg) -> Ptr (RsHashMap RsString RsString) -> IO (Ptr RsOMsg)

foreign import ccall
  get_OMsg_xs :: Ptr RsOMsg -> IO (Ptr (RsVec OMsg.RsIMsg))

foreign import ccall
  get_OMsg_ys_ptr :: Ptr RsOMsg -> IO (Ptr (RsHashMap RsString RsString))

data OMsg = OMsg
  { xs :: [OMsg.IMsg],
    ys :: Map.Map (PBByteString PBString) (PBByteString PBString)
  }

instance RsDrop RsOMsg where
  dropRs = drop_OMsg

instance RsFFI RsOMsg OMsg where
  fromRs rs = do
    xs <- fromRs @(RsVec OMsg.RsIMsg) @[OMsg.IMsg] =<< get_OMsg_xs rs
    ys <-
      fromRsHashMap @RsString @RsString @(PBByteString PBString) @(PBByteString PBString)
        =<< get_OMsg_ys_ptr rs
    pure $ OMsg xs ys

  toRs (OMsg xs ys) = do
    xs <- toRs @(RsVec OMsg.RsIMsg) @[OMsg.IMsg] xs
    ys <- toRsHashMap @RsString @RsString @(PBByteString PBString) @(PBByteString PBString) ys
    new_OMsg xs ys

data RsPocMsg

foreign import ccall
  drop_PocMsg :: Ptr RsPocMsg -> IO ()

foreign import ccall
  new_PocMsg :: Ptr RsString -> IO (Ptr RsPocMsg)

foreign import ccall
  get_PocMsg_xs :: Ptr RsPocMsg -> IO (Ptr RsString)

newtype PocMsg = PocMsg
  { xs :: PBByteString PBString
  }

instance RsDrop RsPocMsg where
  dropRs = drop_PocMsg

instance RsFFI RsPocMsg PocMsg where
  fromRs rs = do
    xs <- fromRs @RsString @(PBByteString PBString) =<< get_PocMsg_xs rs
    pure $ PocMsg xs

  toRs (PocMsg xs) = do
    xs <- toRs @RsString @(PBByteString PBString) xs
    new_PocMsg xs
