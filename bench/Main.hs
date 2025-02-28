{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Crypto.KDF.BIP39

main :: IO ()
main = defaultMain [
    bgroup "ppad-bip39" [
        bench_mnemonic
      , bench_seed
    ]
  ]

bench_mnemonic :: Benchmark
bench_mnemonic = bench "mnemonic" $ nf mnemonic "my ultra secret entropy!"

bench_seed :: Benchmark
bench_seed = bench "seed" $ nf (seed mnem) mempty where
  mnem = "hope simple bubble suggest elbow correct limb hole gloom nasty fringe dolphin finger demand situate unlock junior autumn"

