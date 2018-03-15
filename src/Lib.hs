module Lib
    ( testProcess
    ) where

import System.Process
import GHC.IO.Handle
import Control.Concurrent

testProcess :: IO ()
testProcess = do
  -- (Just hin, , _, p) <- createProcess (proc "mpv" ["--input-terminal=yes", "/home/josh/'IS THAT A MAN RIDING A SHRIMP'-9p_QW_HsKPI.mp3"]){ std_in = CreatePipe, std_out = NoStream, std_err = NoStream}
  (Just hin, Just hout, _, c) <- createProcess (proc "cat" []){ std_in = CreatePipe , std_out = CreatePipe}
  hSetBuffering hin NoBuffering
  threadDelay 2000000
  hPutChar hin 'p'
  hPutChar hin '\n'
  threadDelay 2000000
  hPutChar hin 'p'
  hPutChar hin '\n'
  threadDelay 2000000
  hPutChar hin 'p'
  hPutChar hin '\n'
  hClose hin
  waitForProcess c
  b <- hGetLine hout
  putStrLn (show b)
