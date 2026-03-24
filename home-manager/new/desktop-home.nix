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
      name = "dracula";
    };
    iconTheme = {
      name = "candy-icons";
    };

    gtk4.theme.name = "dracula";
    gtk4.iconTheme.name = "candy-icons";
  };
  
  # :NOTE: setup theming for libadwaita (for nautilus)
  dconf.settings = {
    "org/gnome/desktop/interface" = {
      gtk-theme = "dracula";
      icon-theme = "candy-icons";
      color-scheme = "prefer-dark";
    };
    "org/gnome/nautilus/preferences" = {
      default-folder-viewer = "list-view";
    };
  };
  
  home.file.".xmobarrc".source = "${my-dotfile-dir}/xmobarrc";
  
  home.sessionVariables = {
    MY_MACHINE_ID = "Desktop";
  };

}
