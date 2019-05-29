with import <nixpkgs> { system = "i686-linux"; };
stdenv.mkDerivation {
  name = "i686-shell-for-haskell";
  buildInputs = [
    (pkgsi686Linux.haskell.packages.ghc865.ghcWithPackages (pkgs: with pkgs; [ base ]))
  ];
}
