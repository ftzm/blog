{ pkgs }:

rec {
  generator = pkgs.haskellPackages.developPackage {
    root = ./.;
    modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
        buildTools = with pkgs; (attrs.buildTools or []) ++ [
          haskellPackages.cabal-install
          haskellPackages.hpack
          haskellPackages.ghcid
          zlib
        ];
        configureFlags = [
          "--extra-lib-dirs=${pkgs.zlib}/lib"
        ];
      }) ;
  };
  files = pkgs.stdenv.mkDerivation {
    name = "ftzm-blog";
    src = ./.;
    phases = "unpackPhase buildPhase";
    version = "0.1";
    buildInputs = [ generator ];
    buildPhase = ''
      mkdir $out
      export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive";
      export LANG=en_US.UTF-8
      build-site
      cp -r docs/* $out
    '';
  };
}
