{ pkgs, config, lib, ...}: 
let
  constants = import ./common-constants.nix; 
  sweetIconsRepo = builtins.fetchGit {
    url = "https://github.com/EliverLara/Sweet-folders";
    rev = "40a5d36e50437901c7eaa1119bb9ae8006e2fe5c";
  };
  draculaWallpaperRepo = builtins.fetchGit {
    url = "https://github.com/dracula/wallpaper";
    rev = "f2b8cc4223bcc2dfd5f165ab80f701bbb84e3303";
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

  home.file.".vimrc".source = "${my-dotfile-dir}/vim/vim/vimrc";
  home.file.".Xresources".source = "${my-dotfile-dir}/Xresources";
  home.file.".xmobarrc".source = "${my-dotfile-dir}/xmobarrc-laptop";
  home.file.".xmonad/xmonad.hs".source = "${my-dotfile-dir}/.xmonad/xmonad.hs";
  home.file.".xmonad/lib/MyTheme.hs".source = "${my-dotfile-dir}/.xmonad/MyTheme.hs";
  home.file.".ghci".source = "${my-dotfile-dir}/.ghci";
  home.file.".ripgreprc".source = "${my-dotfile-dir}/.ripgreprc";
  home.file.".config/mpv/mpv.conf".source = "${my-dotfile-dir}/mpv.conf";
  home.file.".config/ytfzf/conf.sh".source = "${my-dotfile-dir}/ytfzf-conf.sh";
  home.file.".config/termonad/termonad.hs".source = "${my-dotfile-dir}/.termonad/termonad.hs";
  home.file.".config/redshift.conf".source = "${my-dotfile-dir}/redshift.conf";
 
  home.file.".config/rofi/themes/dracula-theme.rasi".text = builtins.readFile(builtins.fetchurl {
    url = "https://raw.githubusercontent.com/dracula/rofi/48a024639fbf25e3237766f0dcef4af75a2df908/theme/config1.rasi";
    sha256 = "52f26dd7c44bb919a7a604d71bea26df5e52bd2188f9804e103fc002239bc99a";
  });

  home.file.".config/rofi/themes/dracula-theme-2.rasi".text = builtins.readFile(builtins.fetchurl {
    url = "https://raw.githubusercontent.com/dracula/rofi/48a024639fbf25e3237766f0dcef4af75a2df908/theme/config2.rasi";
    sha256 = "68010556ad7b351b63b6d061f5c4b7c8feb9d9b32687bf0530b105a86634766c";
  });

  home.file.".config/rofi/themes/games-default.rasi".text = builtins.readFile(builtins.fetchurl {
    url = "https://raw.githubusercontent.com/Rolv-Apneseth/rofi-games/17e53726e8f81f6bbe15b4dd66329f869409f4c6/themes/games-default.rasi";
    sha256 = "sha256:0w3cyp7v102n5pdmngz1rhzh86rrc120gfxzxv3h86vs1ch2zli2";
  });

  home.file.".config/rofi/themes/games-smaller.rasi".text = builtins.readFile(builtins.fetchurl {
    url = "https://raw.githubusercontent.com/Rolv-Apneseth/rofi-games/17e53726e8f81f6bbe15b4dd66329f869409f4c6/themes/games-smaller.rasi";
    sha256 = "sha256:1105kf4q9flrdpcf0mzzav6h8m2maj9yfrm9avzwkdfcddax6i7s";
  });

  home.file.".config/kak/colors/dracula.kak".text = builtins.readFile(builtins.fetchurl {
    url = "https://raw.githubusercontent.com/dracula/kakoune/master/colors/dracula.kak";
    sha256 = "57e11ca24375df2a02541a44670d4a48cf90ce179f4eecfc0f5ba005c2c03a02";
  });

  home.file.".themes/dracula".source = builtins.fetchGit {
    url = "https://github.com/dracula/gtk";
    rev = "f3c876d8c97f9bb504c98592a8d96770e70585bb";
  };

  home.file.".themes/sweet".source = builtins.fetchTarball {
    url = "https://github.com/EliverLara/Sweet/releases/download/v6.0/Sweet.tar.xz";
    sha256 = "sha256:012nc14vz31zq34pg5nv2ybzryhqpf6x0jh022nvgyhrf9q1zrn9";
  };

  home.file.".themes/sweet-dark".source = builtins.fetchTarball {
    url = "https://github.com/EliverLara/Sweet/releases/download/v6.0/Sweet-Dark.tar.xz";
    sha256 = "sha256:1j67jwpab7f13rgcxmzqqvjixp5j8mdpv6vpsf2ajnh5yyxzqxvj";
  };

  home.file.".local/share/icons/candy-icons".source = builtins.fetchGit {
    url = "https://github.com/EliverLara/candy-icons";
    rev = "83512fbcadcb7e1015ebbe1729a1894946b021be";
  };

  home.file.".local/share/icons/Sweet-Purple" = {
    source = "${sweetIconsRepo}/Sweet-Purple";
    recursive = true;
  };

  home.file.".local/share/icons/Sweet-Rainbow" = {
    source = "${sweetIconsRepo}/Sweet-Rainbow";
    recursive = true;
  };

  # :NOTE: this sets the desktop wallpaper
  home.file.".background-image" = {
    source = "${draculaWallpaperRepo}/first-collection/nixos.png";
  };

  home.file.".local/share/xfce4/terminal/colorschemes/Dracula.theme".source = builtins.fetchurl {
    url = "https://raw.githubusercontent.com/dracula/xfce4-terminal/refs/heads/master/Dracula.theme";
  };
  
  home.file.".config/lazygit/theme/lazygit".source = builtins.fetchGit {
    url = "https://github.com/catppuccin/lazygit";
    rev = "a544cef9a18c3a94e0344281e0ddcf99a18a8ede";
  };
  home.file.".config/lazygit/config.yml".text = ''
  '';

  home.file.".config/alacritty/dracula.toml".text = builtins.readFile(builtins.fetchurl {
    url = "https://raw.githubusercontent.com/dracula/alacritty/master/dracula.toml";
    sha256 = "e9de3a792548c8112168c1dd18b5651d1ebee2893975cda4ccd9c4c0430c87b8";
  });

  home.file."piper/models/en_US-lessac-high.onnx".source = builtins.fetchurl {
    url = "https://huggingface.co/rhasspy/piper-voices/resolve/main/en/en_US/lessac/high/en_US-lessac-high.onnx?download=true";
    sha256 = "4cabf7c3a638017137f34a1516522032d4fe3f38228a843cc9b764ddcbcd9e09";
  };

  home.file."piper/models/en_US-lessac-high.onnx.json".text = builtins.readFile(builtins.fetchurl {
    url = "https://huggingface.co/rhasspy/piper-voices/raw/main/en/en_US/lessac/high/en_US-lessac-high.onnx.json";
    sha256 = "0bs1j8d97v6bsvfp82h50a23kckz1scfvf312ny5gwjrk1yvjhnv";
  });

  home.file.".PowershellEditorServices".source = pkgs.fetchzip {
    url = "https://github.com/PowerShell/PowerShellEditorServices/releases/download/v3.20.0/PowerShellEditorServices.zip";
    sha256 = "XzyspX6U9FWglDA8VIZE4JamGsFvARQX7iCcQ/blbUE=";
    stripRoot = false;
  };

  # :NOTE: use image profile picture (for display manager)
  home.file.".face".source = ../../img/dracula-profile.png;
  
  home.sessionVariables = {
    MY_MACHINE_ID = "Laptop";
  };

}
