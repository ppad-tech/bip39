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
import qualified Data.Text.ICU.Normalize2 as ICU
import System.IO.Unsafe (unsafePerformIO)

-- XX get rid of this type
newtype Mnemonic = Mnemonic BS.ByteString
  deriving Eq

instance Show Mnemonic where
  show (Mnemonic bs) = show bs

newtype Wordlist = Wordlist (PA.Array T.Text)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

mnemonic :: BS.ByteString -> T.Text
mnemonic = _mnemonic english

_mnemonic :: Wordlist -> BS.ByteString -> T.Text
_mnemonic (Wordlist wlist) entropy@(BI.PS _ _ l)
  | l `rem` 4 /= 0 = error "ppad-bip39 (mnemonic): invalid entropy length"
  | l < 16 = error "ppad-bip39 (mnemonic): invalid entropy length"
  | l > 32 = error "ppad-bip39 (mnemonic): invalid entropy length"
  | otherwise =
      let has = SHA256.hash entropy
          h   = BU.unsafeHead has
          n   = l `quot` 4
          kek = h .&. (0b1111_1111 .<<. (8 - n)) -- top n bits
          cat = entropy <> BS.singleton kek
      in  T.intercalate " " (words wlist cat)
{-# INLINE _mnemonic #-}

-- remaining, bits pool, number of bits in pool
type Acc = (BS.ByteString, Word64, Int)

words :: PA.Array T.Text -> BS.ByteString -> [T.Text]
words wlist bs = L.unfoldr coalg (bs, 0, 0) where
  mask = 0b0111_1111_1111
  coalg :: Acc -> Maybe (T.Text, Acc)
  coalg (etc, acc, len)
    | len > 10 =
        let w11  = fi ((acc .>>. (len - 11)) .&. mask) -- take bits from pool
            nacc = acc .&. ((1 .<<. (len - 11)) - 1)   -- adjust pool
            nlen = len - 11                            -- track less bits
            word = PA.indexArray wlist w11
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

-- XX check that this is a valid mnemonic!
seed :: T.Text -> T.Text -> BS.ByteString
seed mnem pass = PBKDF.derive SHA512.hmac bs salt 2048 64 where
  salt = TE.encodeUtf8 ("mnemonic" <> ICU.nfkd pass)
  bs = TE.encodeUtf8 (ICU.nfkd mnem)

-- wordlists ------------------------------------------------------------------

english :: Wordlist
english = unsafePerformIO $ do
  wlist <- fmap TE.decodeUtf8 (BS.readFile "etc/english.txt")
  let ls = T.lines wlist
  pure (Wordlist (PA.arrayFromList ls))
{-# NOINLINE english #-}

chinese_traditional :: Wordlist
chinese_traditional = unsafePerformIO $ do
  wlist <- fmap TE.decodeUtf8 (BS.readFile "etc/chinese_traditional.txt")
  let ls = T.lines wlist
  pure (Wordlist (PA.arrayFromList ls))
{-# NOINLINE chinese_traditional #-}

chinese_simplified :: Wordlist
chinese_simplified = unsafePerformIO $ do
  wlist <- fmap TE.decodeUtf8 (BS.readFile "etc/chinese_simplified.txt")
  let ls = T.lines wlist
  pure (Wordlist (PA.arrayFromList ls))
{-# NOINLINE chinese_simplified #-}

korean :: Wordlist
korean = unsafePerformIO $ do
  wlist <- fmap TE.decodeUtf8 (BS.readFile "etc/korean.txt")
  let ls = T.lines wlist
  pure (Wordlist (PA.arrayFromList ls))
{-# NOINLINE korean #-}

french :: Wordlist
french = unsafePerformIO $ do
  wlist <- fmap TE.decodeUtf8 (BS.readFile "etc/french.txt")
  let ls = T.lines wlist
  pure (Wordlist (PA.arrayFromList ls))
{-# NOINLINE french #-}

spanish :: Wordlist
spanish = unsafePerformIO $ do
  wlist <- fmap TE.decodeUtf8 (BS.readFile "etc/spanish.txt")
  let ls = T.lines wlist
  pure (Wordlist (PA.arrayFromList ls))
{-# NOINLINE spanish #-}

czech :: Wordlist
czech = unsafePerformIO $ do
  wlist <- fmap TE.decodeUtf8 (BS.readFile "etc/czech.txt")
  let ls = T.lines wlist
  pure (Wordlist (PA.arrayFromList ls))
{-# NOINLINE czech #-}

italian :: Wordlist
italian = unsafePerformIO $ do
  wlist <- fmap TE.decodeUtf8 (BS.readFile "etc/italian.txt")
  let ls = T.lines wlist
  pure (Wordlist (PA.arrayFromList ls))
{-# NOINLINE italian #-}

portuguese :: Wordlist
portuguese = unsafePerformIO $ do
  wlist <- fmap TE.decodeUtf8 (BS.readFile "etc/portuguese.txt")
  let ls = T.lines wlist
  pure (Wordlist (PA.arrayFromList ls))
{-# NOINLINE portuguese #-}

japanese :: Wordlist
japanese = unsafePerformIO $ do
  wlist <- fmap TE.decodeUtf8 (BS.readFile "etc/japanese.txt")
  let ls = T.lines wlist
  pure (Wordlist (PA.arrayFromList ls))
{-# NOINLINE japanese #-}

