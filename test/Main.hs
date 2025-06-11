{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Crypto.HDKey.BIP32 as BIP32
import qualified Crypto.KDF.BIP39 as BIP39
import qualified Data.Aeson as A
import qualified Data.Maybe as M
import qualified Data.Text.ICU.Normalize2 as ICU
import qualified Data.Text.IO as TIO
import Test.Tasty
import Test.Tasty.HUnit
import qualified Vectors as V

data Wordlist =
    English
  | ChineseTraditional
  | ChineseSimplified
  | Czech
  | French
  | Italian
  | Japanese
  | Korean
  | Portuguese
  | Spanish

main :: IO ()
main = do
  vectors_bip39 <- TIO.readFile "etc/vectors.json"
  vectors_jp_bip39 <- TIO.readFile "etc/test_JP_BIP39.json"
  let vectors = do
        a <- A.decodeStrictText vectors_bip39 :: Maybe V.Vectors
        b <- A.decodeStrictText vectors_jp_bip39 :: Maybe [V.JPBip39Test]
        pure (a, b)
  case vectors of
    Nothing -> error "couldn't parse bip39 vectors"
    Just (vs, js) -> defaultMain $
      testGroup "ppad-bip39" [
          bip39_tests vs
        , jp_bip39_tests js
        ]

jp_bip39_tests :: [V.JPBip39Test] -> TestTree
jp_bip39_tests jp_vectors =
  testGroup "jp bip39 vectors" (fmap execute_jp jp_vectors)

bip39_tests :: V.Vectors -> TestTree
bip39_tests V.Vectors {..} =
  testGroup "bip39 vectors" [
      testGroup "english"
        (fmap (execute English) v_english)
    , testGroup "chinese_traditional"
        (fmap (execute ChineseTraditional) v_chinese_traditional)
    , testGroup "chinese_simplified"
        (fmap (execute ChineseSimplified) v_chinese_simplified)
    , testGroup "french"
        (fmap (execute French) v_french)
    , testGroup "czech"
        (fmap (execute Czech) v_czech)
    , testGroup "italian"
        (fmap (execute Italian) v_italian)
    , testGroup "japanese"
        (fmap (execute Japanese) v_japanese)
    , testGroup "korean"
        (fmap (execute Korean) v_korean)
    , testGroup "portuguese"
        (fmap (execute Portuguese) v_portuguese)
    , testGroup "spanish"
        (fmap (execute Spanish) v_spanish)
    ]

execute :: Wordlist -> V.Bip39Test -> TestTree
execute wlist V.Bip39Test {..} = do
    let entr = bt_entropy
        mnem = bt_mnemonic
        seed = bt_seed
        xprv = bt_xprv
        out_mnem = M.fromJust (BIP39._mnemonic wl entr)
        giv_seed = M.fromJust (seed_fn mnem "TREZOR")
        out_seed = M.fromJust (seed_fn out_mnem "TREZOR")
        out_xprv = case BIP32.master out_seed of
          Just hd -> BIP32.xprv hd
          Nothing -> error "bang (bip32)"
        t_msg = mempty
    testGroup t_msg [
        -- we always output (NFKD) normalized UTF8, but test inputs may not be
        -- normalized in this fashion
        testCase "mnemonic" $ assertEqual mempty (ICU.nfkd mnem) out_mnem
        -- testing from the given mnemonic ensures we're normalizing properly
        -- before seed calculation
      , testCase "seed (from given mnemonic)" $ assertEqual mempty seed giv_seed
      , testCase "seed (from derived mnemonic)" $ assertEqual mempty seed out_seed
      , testCase "xprv" $ assertEqual mempty xprv out_xprv
      ]
  where
    seed_fn = case wlist of
      English -> BIP39.seed
      _ -> BIP39.seed_unsafe
    wl = case wlist of
      English -> BIP39.english
      ChineseTraditional -> BIP39.chinese_traditional
      ChineseSimplified -> BIP39.chinese_simplified
      Czech -> BIP39.czech
      French -> BIP39.french
      Italian -> BIP39.italian
      Japanese -> BIP39.japanese
      Korean -> BIP39.korean
      Portuguese -> BIP39.portuguese
      Spanish -> BIP39.spanish

execute_jp :: V.JPBip39Test -> TestTree
execute_jp V.JPBip39Test {..} = do
  let entr = jp_entropy
      mnem = jp_mnemonic
      pass = jp_passphrase
      seed = jp_seed
      xprv = jp_xprv
      out_mnem = M.fromJust (BIP39._mnemonic BIP39.japanese entr)
      giv_seed = M.fromJust (BIP39.seed_unsafe mnem pass)
      out_seed = M.fromJust (BIP39.seed_unsafe out_mnem pass)
      out_xprv = case BIP32.master out_seed of
        Just hd -> BIP32.xprv hd
        Nothing -> error "bang (bip32, jp)"
  testGroup mempty [
      testCase "mnemonic" $ assertEqual mempty (ICU.nfkd mnem) out_mnem
    , testCase "seed (from given mnemonic)" $ assertEqual mempty seed giv_seed
    , testCase "seed (from derived mnemonic)" $ assertEqual mempty seed out_seed
    , testCase "xprv" $ assertEqual mempty xprv out_xprv
    ]

