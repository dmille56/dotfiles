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

  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      PubkeyAuthentication = true;
      AuthenticationMethods = "publickey";
    };
  };

  users.users.dono.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDSAhZLuuy3jGHr6a+lN4+a8okitxOlUqaVMOgWOf0TJ2uQiBw8bmmXhLNLESVG1uDwKXr81+8nXqrU8GYSl+MRgqN8826Ev9a7Eq5khjsaDb5OmHP9M+TZ9CLVqsmbTGEkXHWLqYjobL/EXrMaVdY7lwAb15DuyqM3mE2WSkocWadKkiQ9zunnlirl9dMursqJuiiyIJ2/hgUiU2P6ioygNXmVkT4Py4Ti5UG0UY9KO7+9vAR093WQUseX9/BSkN66k498JaipwdaQfPRk8VfBuVaxUaGGn7o3KWm5MqSqdGbBH2FXKR03EiV/fjkUJgDCziNTzIYuCi2wc7PsQy6lLonAhiCLpTSKqlXvQRBjxYt7fO9wd3geXLQlvuRVruokkW/PTGH8JVZZQxCupc8CGb4Qkr/NEXzHZM0wxfuh1w8wfWPeHPdb43KdKWs1HjB/5cWhxCSMf2+XnVF8OWqtv/iF0as6r+8Yj0AeHiWo4UuH19vQpwxFQ2ORLuFaXj0= dono@van"
  ];
}
