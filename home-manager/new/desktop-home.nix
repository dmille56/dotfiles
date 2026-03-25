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
  
  home.file.".background-image".source = my-desktop-background-image;
  
  home.file.".xmobarrc".source = "${my-dotfile-dir}/xmobarrc";
  
  home.sessionVariables = {
    MY_MACHINE_ID = "Desktop";
  };

}
