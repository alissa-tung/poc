module PB.Common.Status
  ( GRPCStatus (..),
    Code,
    pattern Ok,
    pattern Cancelled,
    pattern Unknown,
    pattern InvalidArgument,
    pattern DeadlineExceeded,
    pattern NotFound,
    pattern AlreadyExists,
    pattern PermissionDenied,
    pattern ResourceExhausted,
    pattern FailedPrecondition,
    pattern Aborted,
    pattern OutOfRange,
    pattern Unimplemented,
    pattern Internal,
    pattern Unavailable,
    pattern DataLoss,
    pattern Unauthenticated,
  )
where

import Data.ByteString qualified as BS
import Foreign.C.Types

data GRPCStatus = GRPCStatus
  { code :: Code,
    message :: BS.ByteString,
    details :: BS.ByteString
  }

newtype Code = Code CInt

pattern
  Ok,
  Cancelled,
  Unknown,
  InvalidArgument,
  DeadlineExceeded,
  NotFound,
  AlreadyExists,
  PermissionDenied,
  ResourceExhausted,
  FailedPrecondition,
  Aborted,
  OutOfRange,
  Unimplemented,
  Internal,
  Unavailable,
  DataLoss,
  Unauthenticated ::
    Code
pattern Ok = Code 0
pattern Cancelled = Code 1
pattern Unknown = Code 2
pattern InvalidArgument = Code 3
pattern DeadlineExceeded = Code 4
pattern NotFound = Code 5
pattern AlreadyExists = Code 6
pattern PermissionDenied = Code 7
pattern ResourceExhausted = Code 8
pattern FailedPrecondition = Code 9
pattern Aborted = Code 10
pattern OutOfRange = Code 11
pattern Unimplemented = Code 12
pattern Internal = Code 13
pattern Unavailable = Code 14
pattern DataLoss = Code 15
pattern Unauthenticated = Code 16
