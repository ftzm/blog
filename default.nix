{ pkgs }:

rec {
  generator = pkgs.haskellPackages.developPackage {
    root = ./.;
    modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
        buildTools = (attrs.buildTools or []) ++ [
          pkgs.haskellPackages.cabal-install
          pkgs.haskellPackages.hpack
          pkgs.haskellPackages.ghcid
          pkgs.zlib
        ];
        configureFlags = [
          "--extra-lib-dirs=${pkgs.zlib}/lib"
        ];
      }) ;
  };
}
