{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    # Haskell toolchain
    ghc
    cabal-install
    haskellPackages.haskell-language-server
  ];

  # Shell hook for additional environment setup
  shellHook = ''
    echo "Haskell development environment loaded!"
  '';
}
