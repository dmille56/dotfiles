let
  officialDraculaWallpaperRepo = builtins.fetchGit {
    url = "https://github.com/dracula/wallpaper";
    rev = "f2b8cc4223bcc2dfd5f165ab80f701bbb84e3303";
  };
  otherDraculaWallpaperRepo = builtins.fetchGit {
    url = "https://github.com/dracula/wallpaper";
    rev = "f2b8cc4223bcc2dfd5f165ab80f701bbb84e3303";
  };
in
rec {
  my-username = "dono";
  my-home-dir = "/home/${my-username}";
  my-dotfile-dir = "${my-home-dir}/dotfiles";
  my-dotfile-nix-dir = "${my-dotfile-dir}/home-manager/new";
  my-desktop-hostname = "castle";
  my-laptop-hostname = "vladtop";
  my-desktop-background-image = ../../img/dracula-castle-matrix-background.png;
  my-laptop-background-image = "${officialDraculaWallpaperRepo}/first-collection/nixos.png";
  my-default-background-image = "${otherDraculaWallpaperRepo}/OS/Nix (Text).png";
}
