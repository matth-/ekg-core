{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- | Perform 100,000 atomic sample additions using 100 concurrent
-- writers.
module Main where

import Control.Concurrent
import Control.Monad
import Criterion.Main
import System.Metrics.Distribution

main :: IO ()
main = defaultMain [
    bgroup "distribution" [ bench "1" $ nfIO distributionMain ]
  ]

distributionMain :: IO ()
distributionMain = do
    distrib <- new
    locks <- replicateM n newEmptyMVar
    mapM_ (forkIO . work distrib iters) locks
    mapM_ takeMVar locks
  where
    n = 100
    iters = 100000

    work :: Distribution -> Int -> MVar () -> IO ()
    work !_ 0 !lock     = putMVar lock ()
    work distrib i lock = add distrib 1.0 >> work distrib (i - 1) lock
