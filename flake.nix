{
  description = "blog";

  # inputs.nixpkgs.url = "github:NixOS/nixpkgs/f6d0d8c265caed8620d799a0a2817d6f0e757f20";
  # pinned to fix acme
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/19b5ddfbb951013461d39352bf05e6248369d580";
  # For accessing `deploy-rs`'s utility Nix functions
  inputs.deploy-rs.url = "github:serokell/deploy-rs";

  outputs = { self, nixpkgs, deploy-rs }:
    let
      packages = with import nixpkgs { system = "x86_64-linux"; };
          callPackage ./release.nix {};

    in
      {
        packages.x86_64-linux.generator = packages.generator;
        packages.x86_64-linux.files = packages.files;
        defaultPackage.x86_64-linux = packages.generator;
        nixosConfigurations.blog-system = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [ (import ./server.nix { nginxWebRoot = packages.files;})];
        };
        deploy.nodes.blog-system = {
          hostname = "ftzm.org";
          profiles.system = {
            sshUser = "root";
            user = "root";
            path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.blog-system;
          };
        };
        # This is highly advised, and will prevent many possible mistakes
        checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
      };
}
