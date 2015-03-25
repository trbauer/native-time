import System.Time.Native

import Control.Concurrent
import Control.Monad
import Data.Word
import Text.Printf

--------------------
-- Testing code
--

-- tests how fast the various timers update
exampleS :: IO ()
exampleS = tickTest nowSeconds 0
-- test how good the delay is
exampleS2 :: IO ()
exampleS2 = tickTest nowSeconds 1000


exampleC :: IO ()
exampleC = tickTest nowClocks 0

exampleMS :: IO ()
exampleMS = tickTest nowMicros 0



-- testCT :: IO ()
-- testCT = do
--  (ST.TOD s picos) <- ST.getClockTime
--  putStrLn $ show s ++ " s + " ++ printf "%06d us" (picos`div`1000000)
--  testCT

testTickRate :: IO Word64 -> String -> IO ()
testTickRate tick units = tick >>= go
  where fmt = printf $ "%12d " ++ units ++ " (+%d)"
        go last = do
          m <- tick
          putStrLn $ fmt m (m - last)
          go m

-- strange:
--   testTickRateSilent (snd `fmap` timeOpMicros (threadDelay 100)) 32
testTickRateSilent :: IO Word64 -> Int -> IO ()
testTickRateSilent tick k = tick >>= go [] k >>= putStrLn . fmtResult
  where go rts 0 _    = return (reverse rts)
        go rts k last = do
          now <- tick
          now `seq` return ()
          go (now - last : rts) (k - 1) now

        fmtResult :: [Word64] -> String
        fmtResult = concatMap (\t -> printf "%12d " t ++ "\n")

testNS :: IO ()
testNS = testTickRate nowNanos "ns"
testUS :: IO ()
testUS = testTickRate nowMicros "us"
testMS :: IO ()
testMS = testTickRate nowMillis "ms"

testNM :: Word64 -> IO ()
testNM k = do
  s <- nowMicros
  threadDelay (fromIntegral k)
  e <- nowMicros
  putStrLn $ printf "%8d us %12s" (e - s) ("(" ++ printf "%8d" (k - (e - s)) ++ ")")
  testNM k

testOp :: Show a => (Int -> IO a) -> IO ()
testOp op = go 1
  where go :: Int -> IO ()
        go i = do
          st_ns <- nowMillis
          (a,t) <- timeOpMicros (op i)
          en_ns <- nowMillis
          putStrLn $ printf "%12d us" t ++ " " ++ show a ++ " (" ++ show (en_ns - st_ns) ++ " ns)"
          go (i+1)


class Num a => TimeType a where
  fmtTimeType :: a -> String

instance TimeType Double where
  fmtTimeType a = printf "%7.4f" a
instance TimeType Word64 where
  fmtTimeType a = printf "%12d" a

-- calls an op every k micros
tickTest :: TimeType a => IO a -> Int -> IO ()
tickTest op delay = op >>= go
  where go pv = do
          when (delay > 0) $
            threadDelay delay
          a <- op
          putStrLn $ fmtTimeType a ++ "   " ++ fmtTimeType (a - pv)
          go a
