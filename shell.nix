with import
  (fetchTarball {
    name = "nixos-22.11-2022-11-26";
    url = "https://github.com/NixOS/nixpkgs/archive/899e7caf59d1954882a8e2dff45ccc0387c186f6.tar.gz";
    sha256 = "06vj1qw5626yhx5mqy2js0dzyc7zrs73ygxz26049f7cdviwcmkx";
  })
    {};
mkShell rec {
  packages =
    [zlib]
    ++
    (with haskell.packages.ghc924;
      [
        ghc
        cabal-install
      ]
    );
  LD_LIBRARY_PATH = lib.makeLibraryPath packages;
}
