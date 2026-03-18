{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./laptop-hardware-configuration.nix
      ./common-configuration.nix
    ];

  networking.hostName = "nixos"; # Define your hostname.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  services.openssh.enable = false;
}
