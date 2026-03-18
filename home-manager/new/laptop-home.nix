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

  # :NOTE: home file configuration starts here

  home.file.".config/kak/kakrc".text = ''
    colorscheme dracula
  '';

  home.file.".config/helix/config.toml".text = ''
    theme = "dracula"
  '';

  home.file.".config/alacritty/alacritty.toml".text = ''
    general.import = ["~/.config/alacritty/dracula.toml"]

    [font]
    size = 13.0
    
    [font.bold]
    family = "DejaVu Sans Mono"
    style = "Bold"
    
    [font.bold_italic]
    family = "DejaVu Sans Mono"
    style = "Bold Italic"
    
    [font.italic]
    family = "DejaVu Sans Mono"
    style = "Italic"
    
    [font.normal]
    family = "DejaVu Sans Mono"
    style = "Regular"

    [window]
    opacity = 0.8
  '';
  
  home.file.".config/spotifyd/spotifyd.conf".text = ''
    [global]
    username = "donovanm56"
    password_cmd = "pass spotify"
    backend = "pulseaudio"
    cache_path = "${my-home-dir}/.cache/spotifyd"
    max_cache_size = 2000000000
    bitrate = 160
  '';

  home.file.".config/spotify-player/app.toml".source = "${my-dotfile-dir}/.config/spotify-player/app.toml";

  home.file.".config/rofi-games/config.toml".text = ''
    [launchers.steam]
    extra_args = ["-silent"]
  '';
  
  home.sessionVariables = {
    MY_MACHINE_ID = "Laptop";
  };

}
