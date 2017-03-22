{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- | Perform 100,000 atomic increments using 100 concurrent writers.
module Main where

import Control.Concurrent
import Control.Monad
import Criterion.Main
import System.Metrics.Counter

main :: IO ()
main = defaultMain [
    bgroup "counter" [ bench "100-100000" $ nfIO (counterMain 100 100000)
                     , bench "1000-100000" $ nfIO (counterMain 1000 100000)
                     ]
  ]

counterMain :: Int -> Int -> IO ()
counterMain n iters = do
    counter <- new
    locks <- replicateM n newEmptyMVar
    mapM_ (forkIO . work counter iters) locks
    mapM_ takeMVar locks
  where
    work :: Counter -> Int -> MVar () -> IO ()
    work !_ 0 !lock     = putMVar lock ()
    work counter i lock = inc counter >> work counter (i - 1) lock
