# bip39

[![](https://img.shields.io/hackage/v/ppad-bip39?color=blue)](https://hackage.haskell.org/package/ppad-bip39)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-bip39-lightblue)](https://docs.ppad.tech/bip39)

An implementation of [BIP39](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki) mnemonics for deterministic key generation.

## Usage

A sample GHCi session:

```
  > -- pragma and bip32/TIO imports not required; just for illustration
  > :set -XOverloadedStrings
  > import qualified Crypto.HDKey.BIP32 as BIP32
  > import qualified Data.Text.IO as TIO
  >
  > import Crypto.KDF.BIP39
  >
  > -- bring your own entropy, e.g. via System.Entropy
  > -- for now we'll use an ultra-secret source
  > let trop = "my ultra secret entropy!"
  >
  > -- use 'mnemonic' to create a BIP39 mnemonic
  > let Just mnem = mnemonic trop
  > mnem
  "hope simple bubble suggest elbow correct limb hole gloom nasty fringe dolphin finger demand situate unlock junior autumn"
  >
  > -- you can use other wordlists; the bip39 'defaults' are included
  > -- to supply an alternative one, use '_mnemonic' (with a leading underscore)
  > let fr_mnem = _mnemonic french trop
  > let jp_mnem = _mnemonic japanese trop
  >
  > TIO.putStrLn fr_mnem
  frivole reculer bilingue secouer diriger chose ingérer frémir fatal luxueux étonnant défiler éolien crayon réflexe tournage hélium appareil
  >
  > TIO.putStrLn jp_mnem
  ずあん はんらん えんえん へいしゃ けむし きかんしゃ そめる しんせいじ したて ちきゅう さとし けいたい こんだて ぐうせい ひかく もちろん せんげん いやがる
  >
  > -- use 'seed' to create a seed
  > let master_seed = seed mnem mempty
  >
  > -- the second argument is a passphrase, which can be arbitrary UTF-8
  > let phrase_seed = seed jp_mnem "㍍ガバヴァぱばぐゞちぢ十人十色"
  >
  > -- use a seed to derive a bip32 hd wallet
  > let hd_wallet = BIP32.master phrase_seed
```

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/bip39](https://docs.ppad.tech/bip39).

## Security

This library aims at the maximum security achievable in a
garbage-collected language under an optimizing compiler such as GHC, in
which strict constant-timeness can be [challenging to achieve][const].

The implementation within passes the official [BIP39 test
vectors](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki#test-vectors)
for every "default" wordlist, including the additional
Japanese vectors with heavily-normalized passphrases.

This library differs somewhat from most *ppad* libraries
as it includes a somewhat-nonstandard dependency, namely
[text-icu](https://hackage.haskell.org/package/text-icu), which wraps
the [International Components for Unicode](https://icu.unicode.org/)
library that is required for UTF8 normalization.

*text-icu* itself is venerable, is maintained by the Haskell Libraries
Committee on [GitHub](https://github.com/haskell/text-icu), and its FFI
code is simple and easy to audit. The alternatives to including it would
probably be either only to support English wordlists, or to vendor the
icu4c dependency ourselves and create our own FFI bindings to the NFKD
normalization code we care about. The latter may be something I look to
do in a future release.

If you discover any vulnerabilities, please disclose them via
security@ppad.tech.

## Performance

The aim is best-in-class performance for pure, highly-auditable Haskell
code.

Current benchmark figures on an M4 Silicon MacBook Air look like (use
`cabal bench` to run the benchmark suite):

```
  benchmarking ppad-bip39/mnemonic
  time                 813.3 ns   (812.4 ns .. 814.2 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 813.3 ns   (810.4 ns .. 823.2 ns)
  std dev              15.99 ns   (5.116 ns .. 32.29 ns)
  variance introduced by outliers: 23% (moderately inflated)

  benchmarking ppad-bip39/seed
  time                 2.057 ms   (2.053 ms .. 2.063 ms)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 2.062 ms   (2.059 ms .. 2.067 ms)
  std dev              12.53 μs   (7.614 μs .. 23.28 μs)
```

## Development

You'll require [Nix][nixos] with [flake][flake] support enabled. Enter a
development shell with:

```
$ nix develop
```

Then do e.g.:

```
$ cabal repl ppad-bip39
```

to get a REPL for the main library.

[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
[const]: https://www.chosenplaintext.ca/articles/beginners-guide-constant-time-cryptography.html
[secp]: https://git.ppad.tech/secp256k1
