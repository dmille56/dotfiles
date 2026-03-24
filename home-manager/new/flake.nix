# :NOTE: to how pin a package
# 1. go find the revision from nixos hydra (where the package is still passing): https://hyrdra.nixos.org
# 2. add to inputs: YOURPACKAGE-revision.url = "github:NixOS/nixpkgs/GIT-REVISION-HASH"
# 3. add to outputs: YOURPACKAGE-revision
# 4. let YOURPACKAGE-overlay = final: prev: {
#      aider-chat-full = aider-chat-full-revision.legacyPackages.${prev.system}.aider-chat-full;
#    };
# 5. (add the overlays to your modules) 
# ie. modules = [
#     { nixpkgs.overlays = [ YOURPACKAGE-overlay ]; }
# ... rest of your config...
#
# see here for more: https://www.aalbacetef.io/blog/nix-pinning-a-specific-package-version-in-a-flake-using-overlays/
{
  description = "NixOS Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    aider-chat-full-revision.url = "github:NixOS/nixpkgs/2b69405f19c7004b832a7410c8aefa9d859feea3";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ nixpkgs, home-manager, sops-nix, aider-chat-full-revision, ... }:
    let
      const = import (builtins.toPath "/home/dono/dotfiles/home-manager/new/common-constants.nix");
      my-machine-id = "laptop"; # desktop, laptop
      my-host-name =
        if my-machine-id == "laptop" then "${const.my-laptop-hostname}"
        else if my-machine-id == "desktop" then "${const.my-desktop-hostname}"
        else builtins.throw "Unknown host-name: ${my-machine-id}";

      aider-overlay = final: prev: {
        aider-chat-full = aider-chat-full-revision.legacyPackages.${prev.system}.aider-chat-full;
      };
    in
    {

      nixosConfigurations.${my-host-name} = nixpkgs.lib.nixosSystem {
        modules = [
          { nixpkgs.overlays = [ aider-overlay ]; }

          (builtins.toPath "${const.my-dotfile-nix-dir}/${my-machine-id}-configuration.nix")

          # :NOTE: sops-nix is now added at the system level so secrets are
          # decrypted at boot, before any user session starts
          sops-nix.nixosModules.sops

          # make home-manager as a module of nixos
          # so that home-manager configuration will be deployed automatically when executing `nixos-rebuild switch`
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;

            home-manager.users.dono = import (builtins.toPath "${const.my-dotfile-nix-dir}/${my-machine-id}-home.nix");

            home-manager.sharedModules = [
              sops-nix.homeManagerModules.sops
            ];

            # Optionally, use home-manager.extraSpecialArgs to pass arguments to home.nix
          }
        ];
      };
    };
}
