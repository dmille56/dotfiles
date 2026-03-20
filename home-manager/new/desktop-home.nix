{ pkgs, config, lib, ...}: 
let
  constants = import ./common-constants.nix; 
in with constants;
{
  imports = [ ./common-home.nix ];

  home.packages = with pkgs; [
    sensible-utils # :NOTE: added
    xdg-utils # :NOTE: added
    libnotify # :NOTE: added
    wifitui # :NOTE: added
    zoom-us # :NOTE: added
    remmina # :NOTE: added
    xmessage # :NOTE: added
    x2goclient # :NOTE: added

    kdePackages.dolphin
    nautilus
  ];

  # :TODO: make sure this works to set gtk applications themes correctly
  # gtk = {
  #   enable = true;
  #   theme = {
  #     name = "dracula";
  #     # package = pkgs.dracula-theme;
  #   };
  #   iconTheme = {
  #     name = "candy-icons";
  #     # package = pkgs.papirus-icon-theme;
  #   };
  # };
  
  home.sessionVariables = {
    MY_MACHINE_ID = "Desktop";
  };

}
