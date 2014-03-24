#!/usr/bin/env runghc
{-# LANGUAGE NamedFieldPuns #-}

-- | This script runs all Harlan benchmarks.  It is based on a Haskell
-- benchmarking framework called HSBencher.  Its main advantage is
-- that it supports uploading of benchmark timings to a Google Fusion
-- Table.

-- Requires hsbencher >= 0.2

import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import System.Directory
import System.FilePath
import System.Exit
import System.Environment (getArgs)
import System.Process
import GHC.Conc (getNumProcessors)
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace

import HSBencher (defaultMainModifyConfig)
import HSBencher.Types
import HSBencher.Methods (makeMethod)
import HSBencher.Logging (log)
import HSBencher.MeasureProcess
import HSBencher.Utils (runLogged, defaultTimeout)
import Prelude hiding (log)
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMainModifyConfig myconf

all_benchmarks :: [Benchmark DefaultParamMeaning]
all_benchmarks =
  [ mkBenchmark "test/bench-add-vector.kfc" [show sz] defaultCfgSpc  | sz <- [1,3 .. 90] ] ++ 
  [ mkBenchmark "test/bench-nbody.kfc"      [show sz] defaultCfgSpc  | sz <- [1 .. 66] ] ++ 
  [ mkBenchmark "test/bench-mandelbrot.kfc"      [show sz] defaultCfgSpc  | sz <- [1 .. 50] ] ++
  [ mkBenchmark "test/bench-bfs-color.kfc" [show sz, "16"] defaultCfgSpc 
  | sz <- [100, 1000, 10^4, 10^5, 10^5] ] ++
  [ mkBenchmark "test/bench-raytrace.kfc" [show flag, "100"] defaultCfgSpc | flag <- [0,1] ]

-- Default configuration space over which to vary settings:
defaultCfgSpc = Or [gpu]
 where
   cpu = And [ Set (Variant "CPU") (RuntimeEnv "HARLAN_DEVICE" "cpu") ]
   gpu = And [ Set (Variant "GPU") (RuntimeEnv "HARLAN_DEVICE" "gpu") ]


-- | Put it all together as a full HSBencher configuration.
myconf :: Config -> Config
myconf conf =
  conf
   { benchlist = all_benchmarks
   , buildMethods = [ harlanMethod ]
   }

-- | Teach HSBencher how to build Harlan files From HSBencher's
-- perspective, it just runs pre-built binaries compiled with harlanc.
harlanMethod :: BuildMethod
harlanMethod = BuildMethod
      { methodName = "harlanMethod"
      , canBuild = WithExtension ".kfc"
      , concurrentBuild = False
      , setThreads      = Nothing
      , clean = \_ _ _ -> return ()
      , compile = \ _ _ _ targetpath -> do
         log$ tag++" Compiling harlan file: "++targetpath
         runSuccessful " [harlanc] " ("./harlanc "++targetpath)
         return (StandAloneBinary ("./" ++ (takeBaseName targetpath)))
      }
 where
    tag = " [run-benchmarks] "


-- | A simple wrapper for a command that is expected to succeed (and whose output we
-- don't care about).  Throws an exception if the command fails.
runSuccessful :: String -> String -> BenchM [B.ByteString]
runSuccessful tag cmd = do
  (res,lines) <- runLogged tag cmd
  case res of
    ExitError code  -> error$ "expected this command to succeed! But it exited with code "++show code++ ":\n  "++ cmd
    RunTimeOut {}   -> error "Methods.hs/runSuccessful - internal error!"
    RunCompleted {} -> return lines
