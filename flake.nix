{
  description = "pipestatus";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/f6d0d8c265caed8620d799a0a2817d6f0e757f20";

  outputs = { self, nixpkgs }:
    let
      packages = with import nixpkgs { system = "x86_64-linux"; };
          callPackage ./default.nix {};

    in
      {
        packages.x86_64-linux.pipestatus-bin = packages.generator;
        defaultPackage.x86_64-linux = packages.generator;
      };
}
