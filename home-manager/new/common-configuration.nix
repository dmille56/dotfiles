{ pkgs, config, lib, ... }:
let 
  constants = import ./common-constants.nix; 
in with constants;
{
  # :TODO: implement this full still

  # Bootloader.
  boot.loader.systemd-boot.enable = lib.mkDefault true;
  boot.loader.efi.canTouchEfiVariables = lib.mkDefault true;

  networking.hostName = lib.mkDefault "common"; # Define your hostname. :NOTE: make sure to overwrite this in the actual configuration
  networking.wireless.enable = lib.mkDefault true;  # Enables wireless support via wpa_supplicant.

  # Enable networking
  networking.networkmanager.enable = lib.mkDefault true;

  # Set your time zone.
  time.timeZone = lib.mkDefault "America/Los_Angeles";

  # Select internationalisation properties.
  i18n.defaultLocale = lib.mkDefault "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = lib.mkDefault "en_US.UTF-8";
    LC_IDENTIFICATION = lib.mkDefault "en_US.UTF-8";
    LC_MEASUREMENT = lib.mkDefault "en_US.UTF-8";
    LC_MONETARY = lib.mkDefault "en_US.UTF-8";
    LC_NAME = lib.mkDefault "en_US.UTF-8";
    LC_NUMERIC = lib.mkDefault "en_US.UTF-8";
    LC_PAPER = lib.mkDefault "en_US.UTF-8";
    LC_TELEPHONE = lib.mkDefault "en_US.UTF-8";
    LC_TIME = lib.mkDefault "en_US.UTF-8";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = lib.mkDefault true;

  # Enable lightdm Display Manager
  services.xserver.displayManager.lightdm = {
    enable = lib.mkDefault true;

    greeters.gtk = {
      enable = lib.mkDefault true;
      theme = {
        name = lib.mkDefault "Dracula";
        package = lib.mkDefault pkgs.dracula-theme;
      };
      iconTheme = {
        name = lib.mkDefault "Papirus-Dark";
        package = lib.mkDefault pkgs.papirus-icon-theme;
      };
      cursorTheme = {
        name = lib.mkDefault "Numix-Cursor";
        package = lib.mkDefault pkgs.numix-cursor-theme;
      };
    };
  };
}
