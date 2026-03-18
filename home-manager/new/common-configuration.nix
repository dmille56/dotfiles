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

  # Enable the XFCE Desktop Environment (and also XMonad).
  services.xserver.desktopManager.xfce.enable = lib.mkDefault true;
  services.xserver.windowManager.xmonad = {
    enable = lib.mkDefault true;
    enableContribAndExtras = lib.mkDefault true;

    # :NOTE: example of how to add additional haskell modules (for use in XMonad config)
    # extraPackages = haskellPackage: [
    #   haskellPackages.monad-logger
    # ];
  };
  services.displayManager.defaultSession = lib.mkDefault "none+xmonad";

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = lib.mkDefault "us";
    variant = lib.mkDefault "";
    options = lib.mkDefault "ctrl:swapcaps";
  };

  # :TODO: figure out how to setup xkb correctly so that Pause Break key is mapped to Insert correctly
  # services.xserver.xkb.extraLayouts.custom = {
  #   description = "Custom layout (Pause mapped to Insert)";
  #   languages   = [ "eng" ];

  #   # Write the strictly formatted XKB configuration block
  #   symbolsFile = pkgs.writeText "xkb-symbols-custom" ''
  #     default partial alphanumeric_keys modifier_keys
  #     xkb_symbols "custom" {
  #       // Inherit the explicit base variant
  #       include "us(basic)"

  #       // Remap the Pause keycode <PAUS> to the Insert keysym
  #       // Override the default PC level typing to ensure it applies cleanly
  #       key <PAUS> { type="ONE_LEVEL", symbols[Group1] = [ Insert ] };
  #     };
  #   '';
  # };

  # services.xserver.xkb.options = "ctrl:swapcaps";
  # services.xserver.xkb.layout  = "custom";
  # services.xserver.xkb.variant  = "";

  # Configure bluetooth
  hardware.bluetooth = {
    enable = lib.mkDefault true;
    powerOnBoot = lib.mkDefault true;
  };

  services.blueman.enable = lib.mkDefault true;

  # Enable CUPS to print documents.
  services.printing.enable = lib.mkDefault true;

  # Enable sound with pipewire.
  services.pulseaudio.enable = lib.mkDefault false;
  security.rtkit.enable = lib.mkDefault true;
  services.pipewire = {
    enable = lib.mkDefault true;
    alsa.enable = lib.mkDefault true;
    alsa.support32Bit = lib.mkDefault true;
    pulse.enable = lib.mkDefault true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  nixpkgs.overlays = [
    (import (builtins.fetchGit {
      url = "https://github.com/nix-community/emacs-overlay.git";
      rev = "fe3b2b9eeef2150992104730612230bbe061dca3";
    }))
  ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.dono = {
    isNormalUser = true;
    description = "dono";
    extraGroups = [ "networkmanager" "wheel" ];
    shell = pkgs.zsh;
    homeMode = "711";
  };

  # Install firefox.
  programs.firefox.enable = lib.mkDefault true;
  programs.steam.enable = lib.mkDefault true;
  programs.zsh.enable = lib.mkDefault true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = lib.mkDefault true;

  # Enable flakes
  nix.settings.experimental-features = lib.mkDefault [ "nix-command" "flakes" ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    neovim
    wget
    curl
    w3m
  ];

  environment.sessionVariables = {
   BROWSER = lib.mkDefault "firefox";
   EDITOR = lib.mkDefault "nano";
  };

  services.upower.enable = lib.mkDefault true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  services.pcscd.enable = lib.mkDefault true;
  programs.gnupg.agent = {
    enable = lib.mkDefault true;
    enableSSHSupport = lib.mkDefault true;
  };

}
