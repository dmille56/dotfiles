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

  environment.systemPackages = with pkgs; [
    x11vnc
  ];

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  services.openssh = {
    enable = true;
    ports = [ 50022 ];
  };

  networking.firewall.allowedTCPPorts = [ 50022 ];

  # Scale everything on X11 (96 is default). Try 144 or 192.
  services.xserver.dpi = 144;

  # Bigger cursor everywhere (including login manager)
  environment.variables.XCURSOR_SIZE = "48";

  # Remap Pause/Break key to Insert
  services.xserver.displayManager.sessionCommands = ''
    ${pkgs.xmodmap}/bin/xmodmap -e "keycode 127 = Insert"
  '';

  # VNC server via x11vnc as a systemd service
  # :NOTE: No VNC password needed since access is restricted to SSH tunnel only.
  # Only accessible via SSH tunneling:
  #   ssh -p 50022 -L 5900:localhost:5900 dono@your-desktop-ip
  # Then connect your VNC client to localhost:5900
  systemd.services.x11vnc = {
    description = "x11vnc VNC Server";
    after = [ "display-manager.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.x11vnc}/bin/x11vnc -display :0 -forever -shared -nopw -auth /home/${constants.my-username}/.Xauthority -localhost";
      Restart = "on-failure";
      RestartSec = "3";
    };
  };

  # Port 5900 is intentionally not opened in the firewall.
  # VNC is only accessible via SSH tunnel for security.

  # :NOTE: graphics card configuration

  # Enable OpenGL
  hardware.graphics = {
    enable = true;
  };

  # Load nvidia driver for Xorg and Wayland
  services.xserver.videoDrivers = ["nvidia"];

  hardware.nvidia = {

    # Modesetting is required.
    modesetting.enable = true;

    # Nvidia power management. Experimental, and can cause sleep/suspend to fail.
    # Enable this if you have graphical corruption issues or application crashes after waking
    # up from sleep. This fixes it by saving the entire VRAM memory to /tmp/ instead 
    # of just the bare essentials.
    powerManagement.enable = false;

    # Fine-grained power management. Turns off GPU when not in use.
    # Experimental and only works on modern Nvidia GPUs (Turing or newer).
    powerManagement.finegrained = false;

    # Use the NVidia open source kernel module (not to be confused with the
    # independent third-party "nouveau" open source driver).
    # Support is limited to the Turing and later architectures. Full list of 
    # supported GPUs is at: 
    # https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus 
    # Only available from driver 515.43.04+
    open = false;

    # Enable the Nvidia settings menu,
	  # accessible via `nvidia-settings`.
    nvidiaSettings = true;

    # Optionally, you may need to select the appropriate driver version for your specific GPU.
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };
}
