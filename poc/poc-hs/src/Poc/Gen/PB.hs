module Poc.Gen.PB where

import Control.Monad
import Data.Map.Strict qualified as Map
import Foreign
import Foreign.C.Types
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

data OMsg = OMsg
  { xs :: [OMsg.IMsg],
    ys :: Map.Map (PBByteString PBString) (PBByteString PBString)
  }

data RsOMsg

foreign import ccall
  get_OMsg_xs_ptr :: Ptr RsOMsg -> IO (Ptr (Ptr OMsg.RsIMsg))

foreign import ccall
  get_OMsg_xs_len :: Ptr RsOMsg -> IO CSize

foreign import ccall
  get_OMsg_ys_ptr :: Ptr RsOMsg -> IO (Ptr (RsHashMap RsString RsString))

foreign import ccall
  drop_OMsg :: Ptr RsOMsg -> IO ()

foreign import ccall
  new_OMsg ::
    Ptr (Ptr OMsg.RsIMsg) -> CSize -> Ptr (RsHashMap RsString RsString) -> IO (Ptr RsOMsg)

instance RsFFI RsOMsg OMsg where
  fromRs rs = do
    xs <- join $ fromRsVec <$> get_OMsg_xs_ptr rs <*> (fromIntegral <$> get_OMsg_xs_len rs)
    ys <-
      fromRsHashMap @RsString @RsString @(PBByteString PBString) @(PBByteString PBString)
        =<< get_OMsg_ys_ptr rs
    pure $ OMsg xs ys

  toRs (OMsg xs ys) = do
    (xsPtr, xsLen) <- toRsVec xs
    new_OMsg xsPtr xsLen =<< toRsHashMap ys

instance RsDrop RsOMsg where
  dropRs = drop_OMsg

newtype PocMsg = PocMsg
  { xs :: PBByteString PBString
  }
