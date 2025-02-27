{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Crypto.KDF.BIP39 as BIP39
import qualified Data.ByteString as BS
import qualified Data.Aeson as A
import qualified Data.Text.IO as TIO
import Test.Tasty
import Test.Tasty.HUnit
import qualified Vectors as V

main :: IO ()
main = do
  vectors_bip39 <- TIO.readFile "etc/vectors.json"
  let vectors =
        A.decodeStrictText vectors_bip39 :: Maybe V.Vectors
  case vectors of
    Nothing -> error "couldn't parse bip39 vectors"
    Just vs -> defaultMain $ testGroup "ppad-bip39" [
        bip39_tests vs
      ]

bip39_tests :: V.Vectors -> TestTree
bip39_tests V.Vectors {..} =
  testGroup "bip39 vectors" $
    fmap execute v_wordlist

execute :: V.Bip39Test -> TestTree
execute V.Bip39Test {..} = do
  let entr = bt_entropy
      mnem = bt_mnemonic
      seed = bt_seed
      xprv = bt_xprv
      BIP39.Mnemonic out = BIP39.mnemonic entr
      t_msg = "mnemonic " <> show mnem
  testCase t_msg $
    assertEqual mempty mnem out

