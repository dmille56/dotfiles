# :NOTE: Install notes
# 1.) copy this to /etc/nixos/flake.nix
# 2.) change my-machine-id variable to the correct machine
# 3.) make sure dotfiles are cloned to /home/dono/dotfiles
# 4.) sudo nixos-rebuild switch --impure

# :NOTE: how to update nix flakes
# cd 
let 
  const = (import "/home/dono/dotfiles/home-manager/new/common-constants.nix");
  my-machine-id = "laptop"; # desktop, laptop
  my-host-name =
    if my-machine-id == "laptop" then "batmobile"
    else if my-machine-id == "desktop" then "van"
    else builtins.throw "Unknown host-name: ${my-machine-id}";
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

    outputs = inputs@{ nixpkgs, home-manager, sops-nix, ... }: {
      nixosConfigurations = {
        # :NOTE: change the hostname to your own
        "${my-host-name}" = nixpkgs.lib.nixosSystem {
          modules = [
            "${const.my-dotfile-nix-dir}/${my-machine-id}-configuration.nix";
            # ./configuration.nix # :TODO: remove later
            home.file.".xmobarrc".source = "${const.my-dotfile-dir}/xmobarrc-${my-machine-id}"; # :TODO: fix this to work with desktop later
            sops-nix.nixosModules.sops

            # make home-manager as a module of nixos
            # so that home-manager configuration will be deployed automatically when executing `nixos-rebuild switch`
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;

              home-manager.users.dono = (import "${const.my-dotfile-nix-dir}-${my-machine-id}-home.nix";
              # home-manager.users.dono = import ./home.nix; # :TODO: remove later

	      home-manager.sharedModules = [
                sops-nix.homeManagerModules.sops
	      ];

              # Optionally, use home-manager.extraSpecialArgs to pass arguments to home.nix
            }
          ];
        };
      };
    };
}
