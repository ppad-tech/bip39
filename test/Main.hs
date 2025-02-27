{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Crypto.HDKey.BIP32 as BIP32
import qualified Crypto.KDF.BIP39 as BIP39
import qualified Data.Aeson as A
import qualified Data.Text.IO as TIO
import Test.Tasty
import Test.Tasty.HUnit
import qualified Vectors as V

data Wordlist =
    English
  | ChineseTraditional

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
  testGroup "bip39 vectors" [
      testGroup "english"
        (fmap (execute English) v_english)
    , testGroup "chinese_traditional"
        (fmap (execute ChineseTraditional) v_chinese_traditional)
    ]

execute :: Wordlist -> V.Bip39Test -> TestTree
execute wlist V.Bip39Test {..} = do
    let entr = bt_entropy
        mnem = bt_mnemonic
        seed = bt_seed
        xprv = bt_xprv
        BIP39.Mnemonic out_mnem = BIP39._mnemonic wl entr
        out_seed = BIP39.seed out_mnem "TREZOR"
        out_xprv = case BIP32.master out_seed of
          Just hd -> BIP32.xprv hd
          Nothing -> error "bang (bip32)"
        t_msg = "mnemonic " <> show mnem
    testGroup t_msg [
        testCase "mnemonic" $ assertEqual mempty mnem out_mnem
      , testCase "seed" $ assertEqual mempty seed out_seed
      , testCase "xprv" $ assertEqual mempty xprv out_xprv
      ]
  where
    wl = case wlist of
      English -> BIP39.english
      ChineseTraditional -> BIP39.chinese_traditional
