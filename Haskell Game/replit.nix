{ pkgs }: {
    deps = [
      pkgs.ihaskell
        pkgs.haskellPackages.ghc
        pkgs.haskell-language-server
        pkgs.haskellPackages.random
    ];
}