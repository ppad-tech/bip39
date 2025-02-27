{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Vectors (
    Vectors(..)
  , Bip39Test(..)
  , JPBip39Test(..)
  ) where

import Data.Aeson ((.:))
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

data Vectors = Vectors {
    v_english             :: ![Bip39Test]
  , v_chinese_traditional :: ![Bip39Test]
  , v_chinese_simplified  :: ![Bip39Test]
  , v_french              :: ![Bip39Test]
  , v_spanish             :: ![Bip39Test]
  , v_korean              :: ![Bip39Test]
  , v_japanese            :: ![Bip39Test]
  , v_portuguese          :: ![Bip39Test]
  , v_czech               :: ![Bip39Test]
  , v_italian             :: ![Bip39Test]
  } deriving Show

instance A.FromJSON Vectors where
  parseJSON = A.withObject "Vectors" $ \m -> Vectors
    <$> m .: "english"
    <*> m .: "chinese_traditional"
    <*> m .: "chinese_simplified"
    <*> m .: "french"
    <*> m .: "spanish"
    <*> m .: "korean"
    <*> m .: "japanese"
    <*> m .: "portuguese"
    <*> m .: "czech"
    <*> m .: "italian"

data Bip39Test = Bip39Test {
    bt_entropy        :: !BS.ByteString
  , bt_mnemonic       :: !T.Text
  , bt_seed           :: !BS.ByteString
  , bt_xprv           :: !BS.ByteString
  } deriving Show

decodehex :: T.Text -> BS.ByteString
decodehex t = case B16.decode (TE.encodeUtf8 t) of
  Nothing -> error "bang (decodehex)"
  Just bs -> bs

instance A.FromJSON Bip39Test where
  parseJSON  = A.withArray "Bip39Test" $ \m ->
    let bt_entropy = case m V.! 0 of
          A.String t -> decodehex t
          _ -> error "bang (entropy)"
        bt_mnemonic = case m V.! 1 of
          A.String t -> t
          _ -> error "bang (mnemonic)"
        bt_seed = case m V.! 2 of
          A.String t -> decodehex t
          _ -> error "bang (seed)"
        bt_xprv = case m V.! 3 of
          A.String t -> TE.encodeUtf8 t
          _ -> error "bang (xprv)"
    in  pure Bip39Test {..}

data JPBip39Test = JPBip39Test {
    jp_entropy :: !BS.ByteString
  , jp_mnemonic :: !T.Text
  , jp_passphrase :: !T.Text
  , jp_seed :: !BS.ByteString
  , jp_xprv :: !BS.ByteString
  } deriving Show

instance A.FromJSON JPBip39Test where
  parseJSON = A.withObject "JPBip39Test" $ \m -> JPBip39Test
    <$> fmap decodehex (m .: "entropy")
    <*> m .: "mnemonic"
    <*> m .: "passphrase"
    <*> fmap decodehex (m .: "seed")
    <*> fmap TE.encodeUtf8 (m .: "bip32_xprv")

