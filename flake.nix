{
  description = "blog";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
  # For accessing `deploy-rs`'s utility Nix functions
  inputs.deploy-rs.url = "github:serokell/deploy-rs";

  outputs = { self, nixpkgs, deploy-rs }:
    let
      packages = with import nixpkgs { system = "x86_64-linux"; };
        callPackage ./release.nix { };

    in {
      packages.x86_64-linux.generator = packages.generator;
      packages.x86_64-linux.files = packages.files;
      defaultPackage.x86_64-linux = packages.generator;
      nixosConfigurations.blog-system = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ (import ./server.nix { nginxWebRoot = packages.files; }) ];
      };
      deploy.nodes.blog-system = {
        hostname = "ftzm.org";
        profiles.system = {
          sshUser = "root";
          user = "root";
          path = deploy-rs.lib.x86_64-linux.activate.nixos
            self.nixosConfigurations.blog-system;
        };
      };
      # This is highly advised, and will prevent many possible mistakes
      checks = builtins.mapAttrs
        (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
    };
}
