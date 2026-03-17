# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./laptop-hardware-configuration.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nixos"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable lightdm Display Manager
  services.xserver.displayManager.lightdm = {
    enable = true;

    greeters.gtk = {
      enable = true;
      theme = {
        name = "Dracula";
        package = pkgs.dracula-theme;
      };
      iconTheme = {
        name = "Papirus-Dark";
        package = pkgs.papirus-icon-theme;
      };
      cursorTheme = {
        name = "Numix-Cursor";
        package = pkgs.numix-cursor-theme;
      };
    };
  };

  # Enable the XFCE Desktop Environment (and also XMonad).
  services.xserver.desktopManager.xfce.enable = true;
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;

    # :NOTE: example of how to add additional haskell modules (for use in XMonad config)
    # extraPackages = haskellPackage: [
    #   haskellPackages.monad-logger
    # ];
  };
  services.displayManager.defaultSession = "none+xmonad";

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
    options = "ctrl:swapcaps";
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
    enable = true;
    powerOnBoot = true;
  };

  services.blueman.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
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
  programs.firefox.enable = true;
  programs.steam.enable = true;
  programs.zsh.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Enable flakes
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

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
   BROWSER = "firefox";
   EDITOR = "nano";
  };

  services.upower.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  services.pcscd.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # List services that you want to enable:

  # :TODO: enable an x11 vnc server
  services.x2goserver.enable = true;

  # Enable the OpenSSH daemon.
  # :TODO: get the ssh enable correctly for both SSH and VNC (through ssh tunnel)
  services.openssh.enable = true;
  services.openssh.settings.X11Forwarding = true;
  services.openssh.settings.PermitRootLogin = "no";

  services.sshguard.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.11"; # Did you read the comment?
}
