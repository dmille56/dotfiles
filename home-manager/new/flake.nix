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
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixpkgs-unstable";
    };

    aider-chat-full-revision.url = "github:NixOS/nixpkgs/2b69405f19c7004b832a7410c8aefa9d859feea3";
    
    # :NOTE: we're pinning ollama because I'm getting tired of updating everytime i update nixpkgs lol
    ollama-revision.url = "github:NixOS/nixpkgs/16c7794d0a28b5a37904d55bcca36003b9109aaa";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-openclaw = {
      url = "github:openclaw/nix-openclaw/64d410666821866c565e048a4d07d6cf5d8e494e";
    };
    
    nix-steipete-tools.url = "github:openclaw/nix-steipete-tools";
    
    llm-agents.url = "github:numtide/llm-agents.nix";

    drawiterm = {
      url = "github:dmille56/drawiterm";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    opentmux = {
      url = "github:dmille56/opentmux";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    jobspy-plugin.url = "github:dmille56/jobspy-plugin";
  };

  outputs = inputs@{ nixpkgs, home-manager, sops-nix, nix-openclaw, aider-chat-full-revision, ollama-revision, drawiterm, llm-agents, opentmux, jobspy-plugin, ... }:
    let
      const = import (builtins.toPath "/home/dono/dotfiles/home-manager/new/common-constants.nix");
      my-machine-id = "desktop"; # desktop, laptop
      my-host-name =
        if my-machine-id == "laptop" then "${const.my-laptop-hostname}"
        else if my-machine-id == "desktop" then "${const.my-desktop-hostname}"
        else builtins.throw "Unknown host-name: ${my-machine-id}";

      aider-overlay = final: prev: {
        aider-chat-full = aider-chat-full-revision.legacyPackages.${prev.system}.aider-chat-full;
      };

      gogcli-overlay = final: prev: {
        gogcli = inputs.nix-steipete-tools.packages.${prev.system}.gogcli;
      };
      
      # :TODO: fix ollama overlay still
      # ollama-overlay = final: prev: {
      #   ollama-cuda = ollama-revision.legacyPackages.${prev.system}.ollama-cuda;
      # };
      
      openclaw-gateway-fixed-overlay = final: prev: {
        openclaw-gateway = final.runCommand "openclaw-gateway-fixed" {
          inherit (prev.openclaw-gateway) meta;
          nativeBuildInputs = [final.gnused];
        } ''
          cp -r ${prev.openclaw-gateway} $out
          chmod -R u+w $out
          for ext_dir in $out/lib/openclaw/extensions/*/; do
            ext_name=$(basename "$ext_dir")
            manifest="$ext_dir/openclaw.plugin.json"
            dist_dir="$out/lib/openclaw/dist/extensions/$ext_name"
            if [ -f "$manifest" ] && [ -d "$dist_dir" ]; then
              cp "$manifest" "$dist_dir/"
            fi
          done
          for wrapper in $out/bin/*; do
            if [ -f "$wrapper" ]; then
              sed -i "s|${prev.openclaw-gateway}|$out|g" "$wrapper"
            fi
          done
        '';
      };
      
    in
    {
      nixosConfigurations.${my-host-name} = nixpkgs.lib.nixosSystem {
        modules = [
          { 
            nixpkgs.overlays = [ 
              aider-overlay
              nix-openclaw.overlays.default
              openclaw-gateway-fixed-overlay
              drawiterm.overlays.default
              gogcli-overlay
              # ollama-overlay
              opentmux.overlays.default
              llm-agents.overlays.default
              jobspy-plugin.overlays.default
            ]; 
          }

          (builtins.toPath "${const.my-dotfile-nix-dir}/${my-machine-id}-configuration.nix")

          # :NOTE: sops-nix is now added at the system level so secrets are
          # decrypted at boot, before any user session starts
          sops-nix.nixosModules.sops
          
          nix-openclaw.nixosModules.openclaw-gateway

          # make home-manager as a module of nixos
          # so that home-manager configuration will be deployed automatically when executing `nixos-rebuild switch`
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;

            home-manager.users.dono = import (builtins.toPath "${const.my-dotfile-nix-dir}/${my-machine-id}-home.nix");

            home-manager.sharedModules = [
              sops-nix.homeManagerModules.sops
              nix-openclaw.homeManagerModules.openclaw
            ];

            # Optionally, use home-manager.extraSpecialArgs to pass arguments to home.nix
          }
        ];
      };
    };
}
