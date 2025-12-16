# copy this to: ~/.config/home-manager/flake.nix
# via:
# cp flake.nix ~/.config/home-manager/flake.nix
# :TODO: figure out how to configure this without needing to copy it to the config/home-manager directory
{
    description = "My Home Manager Flake";

    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
        home-manager = {
            url = "github:nix-community/home-manager";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        nixgl.url = "github:nix-community/nixGL";
        
        sops-nix = {
          url = "github:Mic92/sops-nix";
          inputs.nixpkgs.follows = "nixpkgs";
        };
    };

    outputs = { nixgl, nixpkgs, home-manager, sops-nix, ...}: {
        homeConfigurations = {
            "dono" = home-manager.lib.homeManagerConfiguration {
                pkgs = import nixpkgs {
                  system = "x86_64-linux";
                  overlays = [ 
                    nixgl.overlay

                    (import (builtins.fetchGit {
                      url = "https://github.com/nix-community/emacs-overlay.git";
                      rev = "fe3b2b9eeef2150992104730612230bbe061dca3";
                    }))

                    (self: super: {
                      mpv = super.mpv.override {
                        scripts = [ self.mpvScripts.quality-menu ];
                      };
                    })
                  ];
                };

                modules = [ 
                  sops-nix.homeManagerModules.sops
                  ./home.nix
                ];
            };
        };
    };
}
