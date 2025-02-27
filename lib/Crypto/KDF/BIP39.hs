{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Crypto.HDKey.BIP32
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- [BIP39](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki)
-- mnemonic codes for deterministic key generation, supporting wordlists in
-- multiple languages.

module Crypto.KDF.BIP39 (
  -- * Mnemonic construction and validation
    mnemonic
  , _mnemonic
  , valid
  , _valid

  -- * Seed derivation
  , seed
  , _seed
  , seed_unsafe

  -- * Wordlists
  , Wordlist(..)
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
import qualified Data.Foldable as F
import qualified Data.Maybe as M
import qualified Data.Primitive.Array as PA
import Data.Word (Word64)
import qualified Data.List as L
import Prelude hiding (words)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.ICU.Normalize2 as ICU
import System.IO.Unsafe (unsafePerformIO)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- | A BIP39 wordlist.
newtype Wordlist = Wordlist (PA.Array T.Text)

-- | Generate a BIP39 mnemonic from some entropy, using the default English
--   wordlist.
--
--   The entropy must be at least 128 bits long and at most 256 bits
--   long. Providing invalid entropy will result in an 'ErrorCall'
--   exception.
--
--   >>> import qualified System.Entropy as E
--   >>> trop <- E.getEntropy 16
--   >>> mnemonic trop
--   "coral maze mimic half fat breeze thought club give brass bone snake"
mnemonic
  :: BS.ByteString -- ^ 128-256 bits of entropy
  -> T.Text
mnemonic = _mnemonic english

-- | Generate a BIP39 mnemonic from some entropy, using the provided
--   wordlist.
--
--   The entropy must be at least 128 bits long and at most 256 bits
--   long. Providing invalid entropy will result in an 'ErrorCall'
--   exception.
--
--   >>> import qualified System.Entropy as E
--   >>> trop <- E.getEntropy 16
--   >>> _mnemonic czech trop
--   "naslepo lysina dikobraz slupka beseda rorejs ostraha kobliha napevno blahobyt kazivost jiskra"
_mnemonic
  :: Wordlist
  -> BS.ByteString -- ^ 128-256 bits of entropy
  -> T.Text
_mnemonic (Wordlist wlist) entropy@(BI.PS _ _ l)
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

-- | Derive a master seed from a provided mnemonic and passphrase, where the
--   mnemonic has been generated from the default English wordlist.
--
--   The mnemonic's length and words are validated. If you want to
--   validate the mnemonic's words against a non-English wordlist, use
--   '_seed'.
--
--   >>> let mnem = "coral maze mimic half fat breeze thought club give brass bone snake"
--   >>  let pass = "hunter2"
--   >>> seed mnem pass
--   <512-bit long seed>
seed
  :: T.Text        -- ^ mnemonic
  -> T.Text        -- ^ passphrase (use e.g. "" or 'mempty' if not required)
  -> BS.ByteString -- ^ seed
seed = _seed english

-- | Derive a master seed from a provided mnemonic and passphrase, where the
--   mnemonic has been generated from an arbitrary wordlist.
--
--   The provided mnemonic is checked for validity using '_valid'.
--   Providing an invalid mnemonic will result in an 'ErrorCall'
--   exception.
--
--   >>> let mnem = "coral maze mimic half fat breeze thought club give brass bone snake"
--   >>  let pass = "hunter2"
--   >>> _seed english mnem pass
--   <512-bit long seed>
_seed
  :: Wordlist      -- ^ wordlist
  -> T.Text        -- ^ mnemonic
  -> T.Text        -- ^ passphrase (use e.g. "" or 'mempty' if not required)
  -> BS.ByteString -- ^ seed
_seed wlist mnem pass
  | not (_valid wlist mnem) =
      error "ppad-bip39 (seed): invalid mnemonic"
  | otherwise =
      let salt = TE.encodeUtf8 ("mnemonic" <> ICU.nfkd pass)
          norm = TE.encodeUtf8 (ICU.nfkd mnem)
      in  PBKDF.derive SHA512.hmac norm salt 2048 64 where
{-# INLINE _seed #-}

-- | Derive a master seed from a provided mnemonic and passphrase.
--
--   The mnemonic's length is validated, but its individual words are
--   /not/. This function thus works for every wordlist.
--
--   >>> let mnem = "coral maze mimic half fat breeze thought club give brass bone snake"
--   >>  let pass = "hunter2"
--   >>> seed_unsafe mnem pass
--   <512-bit long seed>
seed_unsafe
  :: T.Text        -- ^ mnemonic
  -> T.Text        -- ^ passphrase (use e.g. "" or 'mempty' if not required)
  -> BS.ByteString -- ^ seed
seed_unsafe mnem pass
  | length (T.words mnem) `notElem` [12, 15, 18, 21, 24] =
      error "ppad-bip39 (seed_unsafe): invalid mnemonic"
  | otherwise =
      let salt = TE.encodeUtf8 ("mnemonic" <> ICU.nfkd pass)
          norm = TE.encodeUtf8 (ICU.nfkd mnem)
      in  PBKDF.derive SHA512.hmac norm salt 2048 64 where

-- | Validate a mnemonic against the default English wordlist.
--
--   Verifies that the mnemonic has a valid length, and that every word
--   is contained in the wordlist.
--
--   >>> valid "coral maze mimic half fat breeze thought club give brass bone snake"
--   True
--   >>> valid "coral maze mimic half fat breeze thought club give brass bone"
--   False
valid
  :: T.Text -- ^ mnemonic
  -> Bool   -- ^ 'True' if valid
valid mnem =
       length ws `elem` [12, 15, 18, 21, 24]
    && all M.isJust (fmap (\word -> F.find (== word) wlist) ws)
  where
    ws = T.words mnem
    Wordlist wlist = english

-- | Validate a mnemonic against a wordlist.
--
--   Verifies that the mnemonic has a valid length, and that every word
--   is contained in the provided wordlist.
--
--   >>> let mnem = "持 樓 粗 殺 承 圖 湧 整 拿 路 式 棋"
--   >>> _valid chinese_traditional mnem
--   True
--   >>> _valid chinese_simplified mnem
--   False
_valid
  :: Wordlist
  -> T.Text   -- ^ mnemonic
  -> Bool     -- ^ 'True' if valid
_valid (Wordlist wlist) mnem =
       length ws `elem` [12, 15, 18, 21, 24]
    && all M.isJust (fmap (\word -> F.find (== word) wlist) ws)
  where
    ws = T.words mnem

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

