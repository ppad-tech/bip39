{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Crypto.KDF.BIP39 (
    Mnemonic(..)
  , mnemonic
  , _mnemonic
  , seed

  -- wordlists
  , english
  , chinese_traditional
  , chinese_simplified
  , czech
  , french
  , korean
  , italian
  , japanese
  , portuguese
  , spanish
  ) where

import qualified Crypto.KDF.PBKDF as PBKDF
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.SHA512 as SHA512
import Data.Bits ((.&.), (.|.), (.>>.), (.<<.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Primitive.Array as PA
import Data.Word (Word64)
import qualified Data.List as L
import Prelude hiding (words)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO.Unsafe (unsafePerformIO)

newtype Mnemonic = Mnemonic BS.ByteString
  deriving Eq

instance Show Mnemonic where
  show (Mnemonic bs) = show bs

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- remaining, bits pool, number of bits in pool
type Acc = (BS.ByteString, Word64, Int)

words :: PA.Array T.Text -> BS.ByteString -> [BS.ByteString]
words wlist bs = L.unfoldr coalg (bs, 0, 0) where
  mask = 0b0111_1111_1111
  coalg :: Acc -> Maybe (BS.ByteString, Acc)
  coalg (etc, acc, len)
    | len > 10 =
        let w11  = fi ((acc .>>. (len - 11)) .&. mask) -- take bits from pool
            nacc = acc .&. ((1 .<<. (len - 11)) - 1)   -- adjust pool
            nlen = len - 11                            -- track less bits
            word = TE.encodeUtf8 (PA.indexArray wlist w11)
        in  Just (word, (etc, nacc, nlen))
    | not (BS.null etc) =
        let next = BU.unsafeHead etc
            rest = BU.unsafeTail etc
            nacc = (acc .<<. 8) .|. fi next -- add bits to pool
            nlen = len + 8                  -- track additional bits
        in  coalg (rest, nacc, nlen)
    | otherwise =
        Nothing
{-# INLINE words #-}

mnemonic :: BS.ByteString -> Mnemonic
mnemonic = _mnemonic english

_mnemonic :: PA.Array T.Text -> BS.ByteString -> Mnemonic
_mnemonic wlist entropy@(BI.PS _ _ l)
  | l `rem` 4 /= 0 = error "ppad-bip39 (mnemonic): invalid entropy length"
  | l < 16 = error "ppad-bip39 (mnemonic): invalid entropy length"
  | l > 32 = error "ppad-bip39 (mnemonic): invalid entropy length"
  | otherwise =
      let has = SHA256.hash entropy
          h   = BU.unsafeHead has
          n   = l `quot` 4
          kek = h .&. (0b1111_1111 .<<. (8 - n)) -- top n bits
          cat = entropy <> BS.singleton kek
      in  Mnemonic (BS.intercalate " " (words wlist cat))
{-# INLINE _mnemonic #-}

seed :: BS.ByteString -> BS.ByteString -> BS.ByteString
seed mnem pass = PBKDF.derive SHA512.hmac mnem salt 2048 64 where
  salt = "mnemonic" <> pass

-- wordlists ------------------------------------------------------------------

english :: PA.Array T.Text
english = unsafePerformIO $ do
  wlist <- fmap TE.decodeUtf8 (BS.readFile "etc/english.txt")
  let ls = T.lines wlist
  pure (PA.arrayFromList ls)
{-# NOINLINE english #-}

chinese_traditional :: PA.Array T.Text
chinese_traditional = unsafePerformIO $ do
  wlist <- fmap TE.decodeUtf8 (BS.readFile "etc/chinese_traditional.txt")
  let ls = T.lines wlist
  pure (PA.arrayFromList ls)
{-# NOINLINE chinese_traditional #-}

chinese_simplified :: PA.Array T.Text
chinese_simplified = unsafePerformIO $ do
  wlist <- fmap TE.decodeUtf8 (BS.readFile "etc/chinese_simplified.txt")
  let ls = T.lines wlist
  pure (PA.arrayFromList ls)
{-# NOINLINE chinese_simplified #-}

korean :: PA.Array T.Text
korean = unsafePerformIO $ do
  wlist <- fmap TE.decodeUtf8 (BS.readFile "etc/korean.txt")
  let ls = T.lines wlist
  pure (PA.arrayFromList ls)
{-# NOINLINE korean #-}

french :: PA.Array T.Text
french = unsafePerformIO $ do
  wlist <- fmap TE.decodeUtf8 (BS.readFile "etc/french.txt")
  let ls = T.lines wlist
  pure (PA.arrayFromList ls)
{-# NOINLINE french #-}

spanish :: PA.Array T.Text
spanish = unsafePerformIO $ do
  wlist <- fmap TE.decodeUtf8 (BS.readFile "etc/spanish.txt")
  let ls = T.lines wlist
  pure (PA.arrayFromList ls)
{-# NOINLINE spanish #-}

czech :: PA.Array T.Text
czech = unsafePerformIO $ do
  wlist <- fmap TE.decodeUtf8 (BS.readFile "etc/czech.txt")
  let ls = T.lines wlist
  pure (PA.arrayFromList ls)
{-# NOINLINE czech #-}

italian :: PA.Array T.Text
italian = unsafePerformIO $ do
  wlist <- fmap TE.decodeUtf8 (BS.readFile "etc/italian.txt")
  let ls = T.lines wlist
  pure (PA.arrayFromList ls)
{-# NOINLINE italian #-}

portuguese :: PA.Array T.Text
portuguese = unsafePerformIO $ do
  wlist <- fmap TE.decodeUtf8 (BS.readFile "etc/portuguese.txt")
  let ls = T.lines wlist
  pure (PA.arrayFromList ls)
{-# NOINLINE portuguese #-}

japanese :: PA.Array T.Text
japanese = unsafePerformIO $ do
  wlist <- fmap TE.decodeUtf8 (BS.readFile "etc/japanese.txt")
  let ls = T.lines wlist
  pure (PA.arrayFromList ls)
{-# NOINLINE japanese #-}

