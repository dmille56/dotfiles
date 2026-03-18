# :NOTE: Install notes
# 1.) copy this to /etc/nixos/flake.nix
# 2.) change my-machine-id variable to the correct machine
# 3.) make sure dotfiles are cloned to /home/dono/dotfiles
# 4.) sudo nixos-rebuild switch --impure

# :NOTE: how to update nix flakes
{
  description = "NixOS Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ nixpkgs, home-manager, sops-nix, ... }:
    let
      const = import (builtins.toPath "/home/dono/dotfiles/home-manager/new/common-constants.nix");
      my-machine-id = "laptop"; # desktop, laptop
      my-host-name =
        if my-machine-id == "laptop" then "${const.my-desktop-hostname}"
        else if my-machine-id == "desktop" then "${const.my-laptop-hostname}"
        else builtins.throw "Unknown host-name: ${my-machine-id}";
    in
    {
      nixosConfigurations.${my-host-name} = nixpkgs.lib.nixosSystem {
        modules = [
          (builtins.toPath "${const.my-dotfile-nix-dir}/${my-machine-id}-configuration.nix")
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
