(import <nixpkgs> {}).haskell.packages.ghc884.callPackage
(
{ mkDerivation, base, hakyll, stdenv }:
mkDerivation {
  pname = "prednaz-github-io";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hakyll ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
)
{}