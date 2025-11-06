# copy this to: ~/.config/nixpkgs/home.nix
# via: cp hm-config.nix ~/.config/nixpkgs/home.nix
{ config, pkgs, inputs, ... }:

{
  imports = [
    /home/dono/dotfiles/home-manager/home.nix
  ];
}
