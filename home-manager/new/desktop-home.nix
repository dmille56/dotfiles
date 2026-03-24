{ pkgs, config, lib, ...}: 
let
  constants = import ./common-constants.nix; 
  gtkDraculaRepo = builtins.fetchGit {
    url = "https://github.com/dracula/gtk";
    rev = "2618a035409d65e0a1e4da1909ae1b5fd6a796fd";
  };
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
  
  # :TODO: make KDE theming work appropriately
  home.file.".local/share/color-schemes/Dracula.colors".source = lib.mkDefault "${gtkDraculaRepo}/kde/color-schemes/Dracula.colors";
  home.file.".local/share/plasma/desktoptheme/dracula".source = lib.mkDefault "${gtkDraculaRepo}/kde/plasma/desktoptheme/Dracula";
  home.file.".local/share/plasma/look-and-feel/dracula".source = lib.mkDefault "${gtkDraculaRepo}/kde/look-and-feel/Dracula";
  
  home.file.".config/kdeglobals".text = lib.mkDefault ''
    [General]
    ColorScheme=Dracula
  
    [KDE]
    LookAndFeelPackage=dracula
  '';
  
  home.file.".config/plasmarc".text = lib.mkDefault ''
    [Theme]
    name=dracula
  '';
  
  home.file.".background-image".source = ../../img/dracula-castle-matrix-background.png;
  
  home.file.".xmobarrc".source = "${my-dotfile-dir}/xmobarrc";
  
  home.sessionVariables = {
    MY_MACHINE_ID = "Desktop";
  };

}
