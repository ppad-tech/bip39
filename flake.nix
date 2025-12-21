{
  description = "Pure Haskell BIP39 hierarchical deterministic wallets.";

  inputs = {
    ppad-nixpkgs = {
      type = "git";
      url  = "git://git.ppad.tech/nixpkgs.git";
      ref  = "master";
    };
    ppad-bip32 = {
      type = "git";
      url  = "git://git.ppad.tech/bip32.git";
      ref  = "master";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
      inputs.ppad-base16.follows = "ppad-base16";
      inputs.ppad-sha256.follows = "ppad-sha256";
      inputs.ppad-sha512.follows = "ppad-sha512";
    };
    ppad-base16 = {
      type = "git";
      url  = "git://git.ppad.tech/base16.git";
      ref  = "master";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
    };
    ppad-sha256 = {
      type = "git";
      url  = "git://git.ppad.tech/sha256.git";
      ref  = "master";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
      inputs.ppad-base16.follows = "ppad-base16";
    };
    ppad-sha512 = {
      type = "git";
      url  = "git://git.ppad.tech/sha512.git";
      ref  = "master";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
      inputs.ppad-base16.follows = "ppad-base16";
    };
    ppad-pbkdf = {
      type = "git";
      url  = "git://git.ppad.tech/pbkdf.git";
      ref  = "master";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
      inputs.ppad-sha256.follows = "ppad-sha256";
      inputs.ppad-sha512.follows = "ppad-sha512";
    };
    flake-utils.follows = "ppad-nixpkgs/flake-utils";
    nixpkgs.follows = "ppad-nixpkgs/nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, ppad-nixpkgs
            , ppad-sha256, ppad-sha512
            , ppad-base16
            , ppad-bip32
            , ppad-pbkdf
            }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        lib = "ppad-bip39";

        pkgs = import nixpkgs { inherit system; };
        hlib = pkgs.haskell.lib;
        llvm = pkgs.llvmPackages_15.llvm;

        bip32 = ppad-bip32.packages.${system}.default;
        bip32-llvm =
          hlib.addBuildTools
            (hlib.enableCabalFlag bip32 "llvm")
            [ llvm ];

        hpkgs = pkgs.haskell.packages.ghc981.extend (new: old: {
          ${lib} = old.callCabal2nixWithOptions lib ./. "--enable-profiling" {};
          ppad-bip32 = bip32-llvm;
          ppad-base16 = ppad-base16.packages.${system}.default;
          ppad-sha256 = ppad-sha256.packages.${system}.default;
          ppad-sha512 = ppad-sha512.packages.${system}.default;
          ppad-pbkdf = ppad-pbkdf.packages.${system}.default;
        });

        cc    = pkgs.stdenv.cc;
        ghc   = hpkgs.ghc;
        cabal = hpkgs.cabal-install;
      in
        {
          packages.default = hpkgs.${lib};

          devShells.default = hpkgs.shellFor {
            packages = p: [
              (hlib.doBenchmark p.${lib})
            ];

            buildInputs = [
              cabal
              cc
              llvm
            ];

            doBenchmark = true;

            shellHook = ''
              PS1="[${lib}] \w$ "
              echo "entering ${system} shell, using"
              echo "cc:    $(${cc}/bin/cc --version)"
              echo "ghc:   $(${ghc}/bin/ghc --version)"
              echo "cabal: $(${cabal}/bin/cabal --version)"
              echo "llc:   $(${llvm}/bin/llc --version | head -2 | tail -1)"
            '';
          };
        }
      );
}

