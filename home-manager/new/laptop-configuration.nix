{ config, pkgs, ... }:

let
  constants = import ./common-constants.nix; 
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./laptop-hardware-configuration.nix
      ./common-configuration.nix
    ];

  networking.hostName = "${constants.my-laptop-hostname}"; # Define your hostname.

  # The ThinkPad Hybrid USB-C dock uses DisplayLink for its monitor outputs.
  # Keep modesetting for the laptop's Intel GPU and add the DisplayLink driver
  # for the dock's USB graphics adapter.
  services.xserver.videoDrivers = [ "modesetting" "displaylink" ];

  # Configure the laptop touchpad through libinput.
  services.xserver.libinput = {
    enable = true;

    touchpad = {
      tapping = true;
      naturalScrolling = true;
      disableWhileTyping = true;
      scrollMethod = "twofinger";
      clickMethod = "clickfinger";
    };
  };

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  services.openssh.enable = false;
}
