{-# LANGUAGE ForeignFunctionInterface #-}
module System.Time.Native(
    timeOpMicros

  , nowClocks
  , nowNanos
  , nowMicros
  , nowMillis
  , nowSeconds

  ) where

import Control.Concurrent
import Control.Exception
import Data.Word
import Text.Printf

foreign import ccall unsafe "time_rdtsc" getRDTSC :: IO Word64
foreign import ccall unsafe "time_ns" getNanos :: IO Word64
foreign import ccall unsafe "time_s" getS :: IO Double

timeOpMicros :: IO a -> IO (a,Word64)
timeOpMicros io = do
  t_st <- nowNanos
  a <- io
  t_en <- nowNanos
  t_diff <- evaluate $! t_en - t_st
  return (a, t_diff`div`1000)

nowClocks :: IO Word64
nowClocks = getRDTSC

nowNanos :: IO Word64
nowNanos = getNanos

nowMicros :: IO Word64
nowMicros = fmap (`div`1000) getNanos

nowMillis :: IO Word64
nowMillis = fmap (`div`1000000) getNanos

nowSeconds :: IO Double
nowSeconds = getS
