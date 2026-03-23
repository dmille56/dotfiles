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

  # :NOTE: GTK theming for GNOME
  # themes are fetched via home.file in common-home.nix to ~/.themes and ~/.local/share/icons
  # if these don't get picked up, add pkgs.dracula-theme / pkgs.papirus-icon-theme as packages
  gtk = {
    enable = true;
    theme = {
      name = "Dracula";
    };
    iconTheme = {
      name = "candy-icons";
    };
  };
  
  home.file.".xmobarrc".source = "${my-dotfile-dir}/xmobarrc";
  
  home.sessionVariables = {
    MY_MACHINE_ID = "Desktop";
  };

}
