{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Vectors (
    Vectors(..)
  , Bip39Test(..)
  ) where

import Data.Aeson ((.:))
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word64)
import qualified Data.Vector as V

data Vectors = Vectors {
    v_wordlist :: ![Bip39Test]
  } deriving Show

instance A.FromJSON Vectors where
  parseJSON = A.withObject "Vectors" $ \m -> Vectors
    <$> m .: "english"

data Bip39Test = Bip39Test {
    bt_entropy        :: !BS.ByteString
  , bt_mnemonic       :: !BS.ByteString
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
          A.String t -> TE.encodeUtf8 t
          _ -> error "bang (mnemonic)"
        bt_seed = case m V.! 2 of
          A.String t -> decodehex t
          _ -> error "bang (seed)"
        bt_xprv = case m V.! 3 of
          A.String t -> TE.encodeUtf8 t
          _ -> error "bang (xprv)"
    in  pure Bip39Test {..}

