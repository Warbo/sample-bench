with import <nixpkgs> {};
with builtins;
with {
  call = haskell.packages.ghc7103.callPackage;
};
haskell.packages.ghc7103.ghcWithPackages (hs: [
  (hs.callPackage (runCabal2nix { url = ./.; }) {})
  (hs.callPackage (runCabal2nix { url = "cabal://criterion-0.8.1.0"; }) {})
])
