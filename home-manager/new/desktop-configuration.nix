{ config, pkgs, ... }:

let
  constants = import ./common-constants.nix; 
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./desktop-hardware-configuration.nix
      ./common-configuration.nix
    ];

  networking.hostName = "${constants.my-desktop-hostname}"; # Define your hostname.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  services.openssh.enable = false;
}
