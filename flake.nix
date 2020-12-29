{
  description = "Hascaf";

  outputs = { self, nixpkgs }: {
    defaultPackage.x86_64-linux =
      with nixpkgs.legacyPackages.x86_64-linux;

      mkShell {
        name = "playgraphics";
        buildInputs = [
          (haskell.packages.ghc8102.ghcWithPackages (ps: with ps; [
            cabal-install
            JuicyPixels
          ]))
        ];
      };
  };
}
