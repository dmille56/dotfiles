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
  ];
  
  home.file.".background-image".source = my-laptop-background-image;
  
  home.sessionVariables = {
    MY_MACHINE_ID = "Laptop";
  };

}
