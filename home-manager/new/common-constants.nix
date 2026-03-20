rec {
  my-username = "dono";
  my-home-dir = "/home/${my-username}";
  my-dotfile-dir = "${my-home-dir}/dotfiles";
  my-dotfile-nix-dir = "${my-dotfile-dir}/home-manager/new";
  # :TODO: make the hostname to castle on desktop
  # my-desktop-hostname = "castle";
  my-desktop-hostname = "nixos";
  my-laptop-hostname = "vladtop";
}
